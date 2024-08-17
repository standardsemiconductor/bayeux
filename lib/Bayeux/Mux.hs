module Bayeux.Mux
  ( MonadMux(..)
  ) where

import Bayeux.Signal

class MonadMux m where
  mux :: Sig Bool -> Sig a -> Sig a -> m (Sig a)
