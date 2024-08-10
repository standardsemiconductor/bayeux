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
sliceRPtr = undefined

sliceRWrap :: Sig (Queue n a) -> Sig Bool
sliceRWrap = undefined

sliceWPtr :: Sig (Queue n a) -> Sig (Finite n)
sliceWPtr = undefined

sliceWWrap :: Sig (Queue n a) -> Sig Bool
sliceWWrap = undefined

sliceDArray :: Sig (Queue n a) -> Sig (Array (Finite n) a)
sliceDArray = undefined

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
