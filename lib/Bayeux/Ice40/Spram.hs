{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Spram
  ( spramC
  ) where

import Bayeux.Rtl

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
