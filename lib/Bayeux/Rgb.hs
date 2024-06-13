{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bayeux.Rgb
  ( prog
  , cycleProg
  ) where

import Bayeux.Rtlil
import Control.Monad.Writer

class Monad m => MonadRgb m where
  ctr :: m SigSpec
--  at  :: SigSpec -> Integer -> m SigSpec
  rgb :: SigSpec -> SigSpec -> SigSpec -> m ()

prog :: MonadRtl m => MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  rgb r g b

class MonadProcess m where
--  process   :: (SigSpec -> m SigSpec) -> m SigSpec
  increment :: SigSpec -> m SigSpec
  eq        :: SigSpec -> SigSpec -> m SigSpec
{-
  -- | If S == 1 then B else A
  mux :: SigSpec   -- ^ S
      -> SigSpec   -- ^ A
      -> SigSpec   -- ^ B
      -> m SigSpec -- ^ Y
-}
cycleProg :: MonadRtl m => MonadProcess m => MonadRgb m => m ()
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

instance MonadRgb Rtl where
  ctr = do
    tell $ counter 32 "\\$my_counter" "\\unused" "$my_counter" "$procStmt"
    return $ SigSpecWireId "\\$my_counter"
{-
  at sigSpec ix = do
    tell
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 1] n
      , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId n) (SigSpecSlice sigSpec ix Nothing)
      ]
    return $ SigSpecWireId n
    where
      n | ix == 24  = "\\pwm_r"
        | ix == 23  = "\\pwm_g"
        | otherwise = "\\pwm_b"
-}
  rgb r g b = do
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\red"
         , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\green"
         , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\blue"
         , ModuleBodyCell $ sbRgbaDrv r g b
         ]

instance MonadProcess Rtl where
{-
  process f = do
    old <- freshWireId
    procStmt <- freshProcStmt
    tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] old]
    srcSig <- f $ SigSpecWireId old
    tell [ModuleBodyProcess $ updateP procStmt
                                (DestSigSpec $ SigSpecWireId old)
                                (SrcSigSpec  $ srcSig)
         ]
    return $ SigSpecWireId old
-}
  increment a = do
    y <- freshWireId
    cId <- freshCellId
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] y
         , ModuleBodyCell $ addC cId False 32 False 32 32 a (SigSpecConstant $ ConstantInteger 1) y
         ]
    return $ SigSpecWireId y

  eq a b = do
    y <- freshWireId
    cId <- freshCellId
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 1] y
         , ModuleBodyCell $ eqC cId False 32 False 32 1 a b y
         ]
    return $ SigSpecWireId y
{-
  mux s a b = do
    y <- freshWireId
    cId <- freshCellId
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] y
         , ModuleBodyCell $ muxC cId 32 a b s y
         ]
    return $ SigSpecWireId y
-}