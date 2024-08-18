module Bayeux.Mux
  ( MonadMux(..)
  ) where

import Bayeux.Rtl hiding (mux)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad.Writer
import Yosys.Rtl

class MonadMux m where
  mux :: Sig a     -- ^ 'False'
      -> Sig a     -- ^ 'True'
      -> Sig Bool
      -> m (Sig a)

instance MonadMux Rtl where
  mux f t s = do
    y   <- freshWire w
    cId <- freshCellId
    tell [ModuleBodyCell $ muxC cId w (spec f) (spec t) (spec s) y]
    return $ Sig y
    where
      w = width f
