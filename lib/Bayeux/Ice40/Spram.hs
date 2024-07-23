{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bayeux.Ice40.Spram
  ( spramC
  , Word14(..)
  , Word4(..)
  , MonadSpram(..)
  , Req(..)
  , rSig
  , wSig
  , memory
  ) where

import Bayeux.Cell
import Bayeux.Encode
import Bayeux.Rtl hiding (process)
import Bayeux.Signal
import Bayeux.Width
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

newtype Word14 = Word14{ unWord14 :: Array (Finite 14) Bool }
  deriving (Encode, Eq, Read, Show, Width)

newtype Word4 = Word4{ unWord4 :: Array (Finite 4) Bool }
  deriving (Encode, Eq, Read, Show, Width)

class MonadSpram m where
  spram :: Sig Word14     -- ^ address
        -> Sig Word16     -- ^ data in
        -> Sig Word4      -- ^ mask write enable
        -> Sig Bool       -- ^ write enable
        -> Sig Bool       -- ^ chip select
        -> Sig Bool       -- ^ stand by
        -> Sig Bool       -- ^ sleep
        -> Sig Bool       -- ^ poweroff
        -> m (Sig Word16) -- ^ data out

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

data Req = R Word14
         | W Word14 Word16 Word4
  deriving (Eq, Read, Show)

instance Width Req where
  width _ = 35

instance Encode Req where
  encode = \case
    R a     -> [B0] <> encode a <> replicate 20 B0
    W a d m -> [B1] <> encode a <> encode d <> encode m

sliceWrEn :: Sig Req -> Sig Bool
sliceWrEn = slice 34 34

sliceAddr :: Sig Req -> Sig Word14
sliceAddr = slice 33 20

sliceDataIn :: Sig Req -> Sig Word16
sliceDataIn = slice 19 4

sliceMaskWrEn :: Sig Req -> Sig Word4
sliceMaskWrEn = slice 3 0

rSig :: Sig Word14 -> Sig Req
rSig a = Sig $ (spec . sig) False <> spec a <> fromString ("20'" <> replicate 20 '0')

wSig :: Sig Word14 -> Sig Word16 -> Sig Word4 -> Sig Req
wSig a d m = Sig $ (spec . sig) True <> spec a <> spec d <> spec m

memory
  :: Monad       m
  => MonadSignal m
  => MonadSpram  m
  => Sig (Maybe Req)
  -> m (Sig (Maybe Word16))
memory reqM = do
  let req  = sliceValue reqM
      wrEn = sliceWrEn req
  b16 <- spram
    (sliceAddr     req)
    (sliceDataIn   req)
    (sliceMaskWrEn req)
    wrEn
    (sliceValid reqM)
    (sig False)
    (sig False)
    (sig True)
  isValid <- process $ const $ logicAnd (sliceValid reqM) =<< logicNot wrEn
  return $ Sig $ spec isValid <> spec b16
