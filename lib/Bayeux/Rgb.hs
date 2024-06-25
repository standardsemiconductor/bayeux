{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Rgb
  ( MonadRgb(..)
  , outputRgb
  , prog
  , cycleProg
  ) where

import Bayeux.Cell ((===))
import qualified Bayeux.Cell as C
import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad.Writer
import Data.Binary
import GHC.Generics

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
  deriving (Binary, Eq, Generic, Read, Show)

instance Width Color where
  width _ = 2

cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  let zero = val (0 :: Word32)
  t <- process $ \timer -> do
    t1Sec <- timer === val 12000000
    timer' <- C.inc timer
    mux t1Sec timer' zero
  tNEqZ <- C.logicNot =<< t === zero
  c <- process $ \color -> do
    cEqBlue <- color === val Blue
    c' <- C.inc color
    ifm [ tNEqZ   `thenm` color
        , cEqBlue `thenm` val Red
        , elsem c'
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
