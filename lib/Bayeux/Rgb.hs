{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bayeux.Rgb
  ( MonadRgb(..)
  , prog
  , cycleProg
  ) where

import Bayeux.Rtlil
import Control.Monad.Writer

class Monad m => MonadRgb m where
  rgb :: SigSpec -> SigSpec -> SigSpec -> m ()

instance MonadRgb Rtl where
  rgb r g b = tell
    [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\red"
    , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\green"
    , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\blue"
    , ModuleBodyCell $ sbRgbaDrv r g b
    ]

increment :: MonadRtl m => SigSpec -> m SigSpec
increment a = binary addC False 32 False 32 32 a $ SigSpecConstant $ ConstantInteger 1

ctr :: MonadRtl m => m SigSpec
ctr = process 32 increment

eq :: MonadRtl m => SigSpec -> SigSpec -> m SigSpec
eq = binary eqC False 32 False 32 1

prog :: MonadRtl m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  rgb r g b

cycleProg :: MonadRtl m => MonadRgb m => m ()
cycleProg = do
  t <- process 32 $ \timer -> do
    t1Sec <- timer `eq` second
    timer' <- increment timer
    mux 32 t1Sec timer' zero
  tEqZ <- t `eq` zero
  c <- process 32 $ \color -> do
    cEqBlue <- color `eq` two
    c' <- increment color
    color' <- mux 32 cEqBlue c' zero
    mux 32 tEqZ color color'
  pwmR <- c `eq` zero
  pwmG <- c `eq` one
  pwmB <- c `eq` two
  rgb pwmR pwmG pwmB
  where
    constSig = SigSpecConstant . ConstantInteger
    zero   = constSig 0
    one    = constSig 1
    two    = constSig 2
    second = constSig 12000000
