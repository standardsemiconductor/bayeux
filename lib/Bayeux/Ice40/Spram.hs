{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Spram
  ( spramC
  , MonadSpram(..)
  ) where

import Bayeux.Rtl
import Bayeux.Signal
import Control.Monad.Writer
import Data.Array
import Data.Finite
import Data.String
import Data.Word

spramC
  :: CellId
  -> SigSpec -- ^ address
  -> SigSpec -- ^ data in
  -> SigSpec -- ^ mask write enable
  -> SigSpec -- ^ write enable
  -> SigSpec -- ^ chip select
  -> SigSpec -- ^ clock
  -> SigSpec -- ^ stand by
  -> SigSpec -- ^ sleep
  -> SigSpec -- ^ power off
  -> SigSpec -- ^ data out
  -> Cell
spramC name a din maskWrEn wren cs clk sb slp pwrOff dout = Cell
  [AttrStmt "\\module_not_derived" $ ConstantInteger 1]
  (CellStmt "\\SB_SPRAM256KA" name)
  [ CellConnect "\\ADDRESS" a
  , CellConnect "\\DATAIN" din
  , CellConnect "\\MASKWREN" maskWrEn
  , CellConnect "\\WREN" wren
  , CellConnect "\\CHIPSELECT" cs
  , CellConnect "\\CLOCK" clk
  , CellConnect "\\STANDBY" sb
  , CellConnect "\\SLEEP" slp
  , CellConnect "\\POWEROFF" pwrOff
  , CellConnect "\\DATAOUT" dout
  ]
  CellEndStmt

class MonadSpram m where
  spram :: Sig (Array (Finite 14) Bool) -- ^ address
        -> Sig Word16                   -- ^ data in
        -> Sig (Array (Finite 4) Bool)  -- ^ mask write enable
        -> Sig Bool                     -- ^ write enable
        -> Sig Bool                     -- ^ chip select
        -> Sig Bool                     -- ^ stand by
        -> Sig Bool                     -- ^ sleep
        -> Sig Bool                     -- ^ poweroff
        -> m (Sig Word16)               -- ^ data out

instance MonadSpram Rtl where
  spram a din maskWrEn wren cs sb slp pwrOff = do
    i    <- fresh
    dout <- freshWire 16
    tell [ModuleBodyCell $ spramC
      (fromString $ "\\SB_SPRAM256KA_INST" <> show i)
      (spec a)
      (spec din)
      (spec maskWrEn)
      (spec wren)
      (spec cs)
      (SigSpecWireId "\\clk")
      (spec sb)
      (spec slp)
      (spec pwrOff)
      dout]
    return $ Sig dout
