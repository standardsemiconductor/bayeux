{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Queue
  ( MonadQueue(..)
  ) where

import Bayeux.Rtl
import Bayeux.Signal
import GHC.TypeNats

class MonadQueue m where
  queue :: Natural -- ^ length
        -> Sig (Maybe a)
        -> m (Sig (Maybe a))

instance MonadQueue Rtl where
  queue n = withNat n queue'


withNat :: forall r. Natural -> (forall n. KnownNat n => SNat n -> r) -> r
withNat n f = withSomeSNat n go
  where
    go :: forall m. SNat m -> r
    go sm = withKnownNat sm $ f sm

queue'
  :: forall n a
   . KnownNat n
  => SNat n
  -> Sig (Maybe a)
  -> Rtl (Sig (Maybe a))
queue' _ = undefined
