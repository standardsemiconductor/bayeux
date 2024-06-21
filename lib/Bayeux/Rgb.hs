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
  rgb :: Sig -- ^ red
      -> Sig -- ^ green
      -> Sig -- ^ blue
      -> m ()

instance MonadRgb Rtl where
  rgb r g b = do
    unless valid $ throwError SizeMismatch
    tell
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\red"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\green"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\blue"
      , ModuleBodyCell $ sbRgbaDrv (spec r) (spec g) (spec b)
      ]
    where
      valid = size r == 1 && size g == 1 && size b == 1

increment :: Monad m => MonadSignal m => Sig -> m Sig
increment a = binary addC a =<< (val . binaryValue) (1 :: Word32)

ctr :: Monad m => MonadSignal m => m Sig
ctr = process False 32 increment

eq :: Monad m => MonadSignal m => Sig -> Sig -> m Sig
eq a = flip at 0 <=< binary eqC a

bar :: Monad m => MonadSignal m => Sig -> m Sig
bar = flip at 0 <=< unary notC

prog :: Monad m => MonadSignal m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  rgb r g b

cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  zero   <- val $ binaryValue (0 :: Word32)
  one    <- val $ binaryValue (1 :: Word32)
  two    <- val $ binaryValue (2 :: Word32)
  second <- val $ binaryValue (12000000 :: Word32)
  t <- process False 32 $ \timer -> do
    t1Sec <- timer `eq` second
    timer' <- increment timer
    mux t1Sec timer' zero
  tNEqZ <- bar =<< t `eq` zero
  c <- process False 32 $ \color -> do
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
