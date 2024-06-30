{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Rgb
  ( MonadRgb(..)
  , outputRgb
  , prog
  , cycleProg
  ) where

import Bayeux.Cell ((===), inc, patm, (~>), wildm)
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad.Writer
import Data.Word

-- | PWM inputs, width=1
class MonadRgb m where
  rgb :: Sig Bool -- ^ red
      -> Sig Bool -- ^ green
      -> Sig Bool -- ^ blue
      -> m (Sig Bool, Sig Bool, Sig Bool)

instance MonadRgb Rtl where
  rgb pwmR pwmG pwmB = do
    red   <- freshWire 1
    green <- freshWire 1
    blue  <- freshWire 1
    tell [ModuleBodyCell $ sbRgbaDrv (spec pwmR) (spec pwmG) (spec pwmB) red green blue]
    return (Sig red, Sig green, Sig blue)

ctr :: Monad m => MonadSignal m => m (Sig Word32)
ctr = process C.inc

prog :: Monad m => MonadSignal m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  outputRgb r g b

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
  outputRgb pwmR pwmG pwmB

-- | Rgb driver with output wires \"red\", \"green\", and \"blue\".
outputRgb
  :: Monad m
  => MonadSignal m
  => MonadRgb m
  => Sig Bool -- ^ red pwm
  -> Sig Bool -- ^ green pwm
  -> Sig Bool -- ^ blue pwm
  -> m ()
outputRgb pwmR pwmG pwmB = do
  (r, g, b) <- rgb pwmR pwmG pwmB
  output "\\red"   r
  output "\\green" g
  output "\\blue"  b
