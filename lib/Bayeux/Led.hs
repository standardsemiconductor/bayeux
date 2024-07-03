{-# LANGUAGE LambdaCase #-}

module Bayeux.Led
  ( MonadLed(..)
  ) where

import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Width

-- | Registers
data Addr = Cr0 -- ^ Control 0
          | Br  -- ^ Pre-scale
          | Onr -- ^ ON time
          | Ofr -- ^ OFF time
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

ledOut :: MonadLed m => MonadRgb => Sig (Maybe (Addr, Word8)) -> m ()
ledOut = rgbOut . slicePwm <=< led
