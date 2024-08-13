{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Queue
  ( MonadQueue(..)
  ) where

import Bayeux.Encode
import Bayeux.Rtl (Rtl)
import Bayeux.Signal
import Bayeux.Width
import Data.Array
import Data.Finite
import Data.Proxy
import GHC.TypeNats

class MonadQueue m where
  queue :: Width a
        => Natural       -- ^ size
        -> Sig (Maybe a) -- ^ enqueue
        -> Sig Bool      -- ^ dequeue
        -> m (Sig (Maybe a))

instance MonadQueue Rtl where
  queue sz = withNat sz queue'


withNat :: forall r. Natural -> (forall n. KnownNat n => SNat n -> r) -> r
withNat n f = withSomeSNat n go
  where
    go :: forall m. SNat m -> r
    go sm = withKnownNat sm $ f sm

data Queue n a = Queue
  { rPtr   :: Finite n
  , rWrap  :: Bool
  , wPtr   :: Finite n
  , wWrap  :: Bool
  , dArray :: Array (Finite n) a
  }
  deriving (Eq, Read, Show)

instance (KnownNat n, Width a) => Width (Queue n a) where
  width _ = 2 + sum
    [ 2 * width (undefined :: Finite n)
    , width (undefined :: Array (Finite n) a)
    ]

instance (KnownNat n, Encode a) => Encode (Queue n a) where
  encode q = mconcat
    [ encode $ rPtr   q
    , encode $ rWrap  q
    , encode $ wPtr   q
    , encode $ wWrap  q
    , encode $ dArray q
    ]

sliceRPtr :: Sig (Queue n a) -> Sig (Finite n)
sliceRPtr = slice (width (undefined :: Finite n) + base - 1) base
  where
    base = width (undefined :: Finite n) - 1 + dArrayWidth + 3

sliceRWrap :: Sig (Queue n a) -> Sig Bool
sliceRWrap = slice
  (width (undefined :: Finite n) - 1 + dArrayWidth + 2)
  (width (undefined :: Finite n) - 1 + dArrayWidth + 2)

rWrapBase :: forall n. KnownNat n => Proxy n -> Integer
rWrapBase _ = width (undefined :: Finite n) - 1 + dArrayWidth (Proxy :: Proxy n) + 2

sliceWPtr :: forall n a. KnownNat n => Sig (Queue n a) -> Sig (Finite n)
sliceWPtr = slice
  (width (undefined :: Finite n) - 1 + dArrayWidth (Proxy :: Proxy n) + 1)
  (dArrayWidth (Proxy :: Proxy n) + 1)

sliceWWrap :: forall n a. KnownNat n => Sig (Queue n a) -> Sig Bool
sliceWWrap = slice (dArrayWidth (Proxy :: Proxy n)) (dArrayWidth (Proxy :: Proxy n))

sliceDArray :: forall n a. KnownNat n => Sig (Queue n a) -> Sig (Array (Finite n) a)
sliceDArray = slice (dArrayWidth (Proxy :: Proxy n) - 1) 0

dArrayWidth :: forall n. KnownNat n => Proxy n -> Integer
dArrayWidth _ = width (undefined :: Array (Finite n) a)

queue'
  :: forall n a
   . KnownNat n
  => Width a
  => SNat n
  -> Sig (Maybe a)
  -> Sig Bool
  -> Rtl (Sig (Maybe a))
queue' _ enqM deq = do
  s <- process $ \(s :: Sig (Queue n a)) -> undefined
  return $ Sig undefined
