{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Rgb
  ( MonadRgb(..)
  , prog
  , cycleProg
  ) where

import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Word

-- | PWM inputs, width=1
class MonadRgb m where
  rgb :: Sig Bool -- ^ red
      -> Sig Bool -- ^ green
      -> Sig Bool -- ^ blue
      -> m ()

instance MonadRgb Rtl where
  rgb r g b = do
    tell
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\red"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\green"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\blue"
      , ModuleBodyCell $ sbRgbaDrv (spec r) (spec g) (spec b)
      ]

increment :: Monad m => MonadSignal m => Sig Word32 -> m (Sig Word32)
increment a = do
  suc <- val (1 :: Word32)
  binary addC a suc

ctr :: Monad m => MonadSignal m => m (Sig Word32)
ctr = process increment

eq :: Monad m => MonadSignal m => Sig Word32 -> Sig Word32 -> m (Sig Bool)
eq a = binary eqC a

bar :: Monad m => MonadSignal m => Sig Bool -> m (Sig Bool)
bar = unary notC

prog :: Monad m => MonadSignal m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  rgb r g b

cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  zero   <- val 0
  one    <- val 1
  two    <- val 2
  second <- val 12000000
  t <- process $ \timer -> do
    t1Sec <- timer `eq` second
    timer' <- increment timer
    mux t1Sec timer' zero
  tNEqZ <- bar =<< t `eq` zero
  c <- process $ \color -> do
    cEqBlue <- color `eq` two
    c' <- increment color
    ifm [ tNEqZ   `thenm` color
        , cEqBlue `thenm` zero
        , elsem c'
        ]
  pwmR <- c `eq` zero
  pwmG <- c `eq` one
  pwmB <- c `eq` two
  rgb pwmR pwmG pwmB
