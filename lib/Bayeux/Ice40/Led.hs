{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Ice40.Led
  ( sbLeddaIp
  , MonadLed(..)
  , outputLed
  , slicePwm
  , ledCtrl
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import Bayeux.Encode
import Bayeux.Ice40.Rgb
import Bayeux.Rtl hiding (mux, process)
import Bayeux.Signal
import Bayeux.Uart
import Bayeux.Width
import Control.Monad
import Control.Monad.Writer
import Data.Array
import Data.Char
import Data.Finite
import Data.Word

sbLeddaIp
  :: SigSpec -- ^ leddcs
  -> SigSpec -- ^ leddclk
  -> SigSpec -- ^ leddat7
  -> SigSpec -- ^ leddat6
  -> SigSpec -- ^ leddat5
  -> SigSpec -- ^ leddat4
  -> SigSpec -- ^ leddat3
  -> SigSpec -- ^ leddat2
  -> SigSpec -- ^ leddat1
  -> SigSpec -- ^ leddat0
  -> SigSpec -- ^ leddaddr3
  -> SigSpec -- ^ leddaddr2
  -> SigSpec -- ^ leddaddr1
  -> SigSpec -- ^ leddaddr0
  -> SigSpec -- ^ ledden
  -> SigSpec -- ^ leddexe
  -> SigSpec -- ^ pwmout0
  -> SigSpec -- ^ pwmout1
  -> SigSpec -- ^ pwmout2
  -> SigSpec -- ^ leddon out
  -> Cell
sbLeddaIp cs clk d7 d6 d5 d4 d3 d2 d1 d0 a3 a2 a1 a0 en exe o0 o1 o2 ledon = Cell
  [AttrStmt "\\module_not_derived" $ ConstantInteger 1]
  (CellStmt "\\SB_LEDDA_IP" "\\SB_LEDDA_IP_INST")
  [ CellConnect "\\LEDDCS" cs
  , CellConnect "\\LEDDCLK" clk
  , CellConnect "\\LEDDDAT7" d7
  , CellConnect "\\LEDDDAT6" d6
  , CellConnect "\\LEDDDAT5" d5
  , CellConnect "\\LEDDDAT4" d4
  , CellConnect "\\LEDDDAT3" d3
  , CellConnect "\\LEDDDAT2" d2
  , CellConnect "\\LEDDDAT1" d1
  , CellConnect "\\LEDDDAT0" d0
  , CellConnect "\\LEDDADDR3" a3
  , CellConnect "\\LEDDADDR2" a2
  , CellConnect "\\LEDDADDR1" a1
  , CellConnect "\\LEDDADDR0" a0
  , CellConnect "\\LEDDDEN" en
  , CellConnect "\\LEDDEXE" exe
  , CellConnect "\\PWMOUT0" o0
  , CellConnect "\\PWMOUT1" o1
  , CellConnect "\\PWMOUT2" o2
  , CellConnect "\\LEDDON" ledon
  ]
  CellEndStmt

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
  cmds <- pats b
    [ Just (w8 'r') ~~> sig (Just $ listArray (0, 2) $ Just <$> [(Pwrr, 0xFF), (Pwrg, 0x00), (Pwrb, 0x00)])
    , Just (w8 'g') ~~> sig (Just $ listArray (0, 2) $ Just <$> [(Pwrr, 0x00), (Pwrg, 0xFF), (Pwrb, 0x00)])
    , Just (w8 'b') ~~> sig (Just $ listArray (0, 2) $ Just <$> [(Pwrr, 0x00), (Pwrg, 0x00), (Pwrb, 0xFF)])
    , wilds $ sig (Nothing :: Maybe (Array (Finite 3) (Maybe (Addr, Word8))))
    ]
  s <- process $ \s -> patm s
    [ (maxBound :: Finite 3) ~> pure s
    , wildm $ inc s
    ]
  outputLed =<< patm s
    [ 0 ~> val (Just (Cr0,  0x80))
    , 1 ~> val (Just (Pwrr, 0xFF))
    , wildm $ joinMaybe =<< cobuffer cmds
    ]
  where
    w8 = fromIntegral . ord
