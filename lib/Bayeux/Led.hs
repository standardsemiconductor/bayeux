{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Bayeux.Led
  ( MonadLed(..)
  , outputLed
  , slicePwm
  , ledCtrl
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import Bayeux.Rgb
import Bayeux.Rtl hiding (mux, process)
import Bayeux.Signal
import Bayeux.Uart
import Control.Monad
import Control.Monad.Writer
import Data.Finitary
import Data.Finite
import Data.Vector.Sized hiding (slice)
import Data.Word

-- | Registers
data Addr = Cr0  -- ^ Control 0
          | Br   -- ^ Pre-scale
          | Onr  -- ^ ON time
          | Ofr  -- ^ OFF time
          | Bcrr -- ^ Breathe on control
          | Bcfr -- ^ Breathe off control
          | Pwrr -- ^ Pulse width for RED
          | Pwrg -- ^ Pulse width for GREEN
          | Pwrb -- ^ Pulse width for BLUE
  deriving (Eq, Read, Show)

instance Finitary Addr where
  type Cardinality Addr = 16

  fromFinite :: Finite (Cardinality Addr) -> Addr
  fromFinite = \case
    8    -> Cr0
    9    -> Br
    10   -> Onr
    11   -> Ofr
    5    -> Bcrr
    6    -> Bcfr
    1    -> Pwrr
    2    -> Pwrg
    3    -> Pwrb
    _    -> error "Not an Addr"

  toFinite :: Addr -> Finite (Cardinality Addr)
  toFinite = \case
    Cr0  -> 8
    Br   -> 9
    Onr  -> 10
    Ofr  -> 11
    Bcrr -> 5
    Bcfr -> 6
    Pwrr -> 1
    Pwrg -> 2
    Pwrb -> 3
{-
instance Encode Addr where
  encode = \case
    Cr0  -> [B1, B0, B0, B0]
    Br   -> [B1, B0, B0, B1]
    Onr  -> [B1, B0, B1, B0]
    Ofr  -> [B1, B0, B1, B1]
    Bcrr -> [B0, B1, B0, B1]
    Bcfr -> [B0, B1, B1, B0]
    Pwrr -> [B0, B0, B0, B1]
    Pwrg -> [B0, B0, B1, B0]
    Pwrb -> [B0, B0, B1, B1]

instance Width Addr where
  width _ = 4
-}
class MonadLed m where
  led :: Sig (Maybe (Addr, Word8))
      -> m (Sig (Bool, Bool, Bool, Bool))

instance MonadLed Rtl where
  led cmd = do
    o0 <- freshWire 1 -- red
    o1 <- freshWire 1 -- green
    o2 <- freshWire 1 -- blue
    ledon <- freshWire 1
    let cs  = "1'1"
        clk = SigSpecWireId "\\clk"
        d   = sliceSnd $ sliceValue cmd
        a   = sliceFst $ sliceValue cmd
        d7  = spec $ slice 7 7 d
        d6  = spec $ slice 6 6 d
        d5  = spec $ slice 5 5 d
        d4  = spec $ slice 4 4 d
        d3  = spec $ slice 3 3 d
        d2  = spec $ slice 2 2 d
        d1  = spec $ slice 1 1 d
        d0  = spec $ slice 0 0 d
        a3  = spec $ slice 3 3 a
        a2  = spec $ slice 2 2 a
        a1  = spec $ slice 1 1 a
        a0  = spec $ slice 0 0 a
        en  = spec $ sliceValid cmd
        exe = "1'1"
    tell [ ModuleBodyCell $
             sbLeddaIp cs
                       clk
                       d7
                       d6
                       d5
                       d4
                       d3
                       d2
                       d1
                       d0
                       a3
                       a2
                       a1
                       a0
                       en
                       exe
                       o0
                       o1
                       o2
                       ledon]
    return $ Sig $ o0 <> o1 <> o2 <> ledon

outputLed
  :: Monad m
  => MonadLed m
  => MonadRgb m
  => MonadSignal m
  => Sig (Maybe (Addr, Word8))
  -> m ()
outputLed = outputRgb . slicePwm <=< led

slicePwm :: Sig (Bool, Bool, Bool, Bool) -> Sig (Bool, Bool, Bool)
slicePwm = slice 3 1

ledCtrl
  :: Monad m
  => MonadLed m
  => MonadBuffer m
  => MonadRgb m
  => MonadSignal m
  => MonadUart m
  => m ()
ledCtrl = do
  b <- receive 624 =<< input "\\rx"
  cmds <- patm (asChar b)
    [ Just 'r' ~> sig red
    , Just 'g' ~> sig green
    , Just 'b' ~> sig blue
    , wildm $ sig (Nothing :: Maybe (Vector 3 (Maybe (Addr, Word8))))
    ]
  cmd <- joinMaybe =<< cobuffer cmds
  s <- process $ \s -> do
    s' <- inc s
    patm s
      [ (maxBound :: Finite 3) ~> s
      , wildm s'
      ]
  outputLed =<< patm s
    [ 0 ~> sig (Just (Cr0,  0x80))
    , 1 ~> sig (Just (Pwrr, 0xFF))
    , wildm $ cmd
    ]
  where
    on  a = Just (a, 0xFF)
    off a = Just (a, 0x00)
    red   = Just $ fromTuple (on  Pwrr, off Pwrg, off Pwrb)
    green = Just $ fromTuple (off Pwrr, on  Pwrg, off Pwrb)
    blue  = Just $ fromTuple (off Pwrr, off Pwrg, on  Pwrb)

asChar :: Sig (Maybe Word8) -> Sig (Maybe Char)
asChar = Sig . spec

joinMaybe :: Finitary a => Monad m => MonadSignal m => Sig (Maybe (Maybe a)) -> m (Sig (Maybe a))
joinMaybe s = do
  v <- sliceValid s `logicAnd` (sliceValid . sliceValue) s
  return $ Sig $ spec v <> (spec . sliceValue . sliceValue) s
