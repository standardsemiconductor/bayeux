{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.IO
  ( sbIO
  ) where

import Yosys.Rtl

sbIO
  :: CellId
  -> Constant -- ^ pin type
  -> Constant -- ^ pullup
  -> Constant -- ^ neg trigger
  -> Constant -- ^ io standard
  -> SigSpec  -- ^ package pin
  -> SigSpec  -- ^ latch input value
  -> SigSpec  -- ^ clock enable
  -> SigSpec  -- ^ input clock
  -> SigSpec  -- ^ output clock
  -> SigSpec  -- ^ output enable
  -> SigSpec  -- ^ D_OUT_0
  -> SigSpec  -- ^ D_OUT_1
  -> SigSpec  -- ^ D_IN_0
  -> SigSpec  -- ^ D_IN_1
  -> Cell
sbIO name pt pu nt ios pp liv ce ci co oe do0 do1 di0 di1 = Cell
  [AttrStmt "\\module_not_derived" $ ConstantInteger 1]
  (CellStmt "\\SB_IO" name)
  [ CellParameter Nothing "\\PIN_TYPE" pt
  , CellParameter Nothing "\\PULLUP" pu
  , CellParameter Nothing "\\NEG_TRIGGER" nt
  , CellParameter Nothing "\\IO_STANDARD" ios
  , CellConnect "\\PACKAGE_PIN" pp
  , CellConnect "\\LATCH_INPUT_VALUE" liv
  , CellConnect "\\CLOCK_ENABLE" ce
  , CellConnect "\\INPUT_CLK" ci
  , CellConnect "\\OUTPUT_CLK" co
  , CellConnect "\\OUTPUT_ENABLE" oe
  , CellConnect "\\D_OUT_0" do0
  , CellConnect "\\D_OUT_1" do1
  , CellConnect "\\D_IN_0" di0
  , CellConnect "\\D_IN_1" di1
  ]
  CellEndStmt
