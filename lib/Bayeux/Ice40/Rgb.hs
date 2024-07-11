{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Rgb
  ( MonadRgb(..)
  , outputRgb
  , prog
  , cycleProg
  ) where

import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad.Writer
import Data.Word

-- | PWM inputs, width=1
class MonadRgb m where
  rgb :: Sig (Bool, Bool, Bool) -- ^ (red, green, blue) pwms
      -> m (Sig Bool, Sig Bool, Sig Bool)

instance MonadRgb Rtl where
  rgb pwmRGB = do
    red   <- freshWire 1
    green <- freshWire 1
    blue  <- freshWire 1
    tell [ModuleBodyCell $ sbRgbaDrv (spec $ slice 2 2 pwmRGB) (spec $ slice 1 1 pwmRGB) (spec $ slice 0 0 pwmRGB) red green blue]
    return (Sig red, Sig green, Sig blue)

ctr :: Monad m => MonadSignal m => m (Sig Word32)
ctr = process C.inc

prog :: Monad m => MonadSignal m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  outputRgb $ Sig $ spec r <> spec g <> spec b

data Color = Red | Green | Blue
  deriving (Eq, Read, Show)

instance Width Color where
  width _ = 2

instance Encode Color where
  encode = \case
    Red   -> [B0, B0]
    Green -> [B0, B1]
    Blue  -> [B1, B0]

cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  t <- process $ \timer -> do
    t' <- inc timer
    patm timer
      [ 12000000 ~> val (0 :: Word32)
      , wildm t'
      ]
  c <- process $ \color -> do
    c' <- inc color
    c'' <- patm color
      [ Blue ~> val Red
      , wildm c'
      ]
    patm t
      [ 12000000 ~> c''
      , wildm color
      ]
  pwmR <- c === val Red
  pwmG <- c === val Green
  pwmB <- c === val Blue
  outputRgb $ Sig $ spec pwmR <> spec pwmG <> spec pwmB

-- | Rgb driver with output wires \"red\", \"green\", and \"blue\".
outputRgb
  :: Monad m
  => MonadSignal m
  => MonadRgb m
  => Sig (Bool, Bool, Bool) -- ^ (red, green, blue) pwm
  -> m ()
outputRgb pwmRGB = do
  (r, g, b) <- rgb pwmRGB
  output "\\red"   r
  output "\\green" g
  output "\\blue"  b
