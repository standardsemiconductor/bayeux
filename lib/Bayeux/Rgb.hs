{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Bayeux.Rgb
  ( MonadRgb(..)
  , outputRgb
  , prog
  , cycleProg
  ) where

import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
import Control.Monad.Writer
import Data.Finitary
import Data.Word
import GHC.Generics (Generic)

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
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Finitary)

cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  t <- process $ \timer -> do
    t' <- inc timer
    patm timer
      [ 12000000 ~> sig (0 :: Word32)
      , wildm t'
      ]
  c <- process $ \color -> do
    c' <- inc color
    c'' <- patm color
      [ Blue ~> sig Red
      , wildm c'
      ]
    patm t
      [ 12000000 ~> c''
      , wildm color
      ]
  pwmR <- c === sig Red
  pwmG <- c === sig Green
  pwmB <- c === sig Blue
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
