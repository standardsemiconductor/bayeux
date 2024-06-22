{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Rgb
  ( MonadRgb(..)
  , prog
  , cycleProg
  ) where

import qualified Bayeux.Cell as C
import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
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

ctr :: Monad m => MonadSignal m => m (Sig Word32)
ctr = process C.inc

prog :: Monad m => MonadSignal m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  rgb r g b

cycleProg :: Monad m => MonadSignal m => MonadRgb m => m ()
cycleProg = do
  let zero = val (0 :: Word32)
      one  = val (1 :: Word32)
      two  = val (2 :: Word32)
      second = val (12000000 :: Word32)
  t <- process $ \timer -> do
    t1Sec <- timer `C.eq` second
    timer' <- C.inc timer
    mux t1Sec timer' zero
  tNEqZ <- C.logicNot =<< t `C.eq` zero
  c <- process $ \color -> do
    cEqBlue <- color `C.eq` two
    c' <- C.inc color
    ifm [ tNEqZ   `thenm` color
        , cEqBlue `thenm` zero
        , elsem c'
        ]
  pwmR <- c `C.eq` zero
  pwmG <- c `C.eq` one
  pwmB <- c `C.eq` two
  rgb pwmR pwmG pwmB
