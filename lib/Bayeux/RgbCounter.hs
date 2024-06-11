{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bayeux.RgbCounter
  ( prog
  , compile
  , cycleProg
  , cycleCompile
  ) where

import Bayeux.Rtlil
import Control.Monad.State
import Control.Monad.Writer
import Data.String

class Monad m => MonadRgb m where
  ctr :: m SigSpec
  at  :: SigSpec -> Integer -> m SigSpec
  rgb :: SigSpec -> SigSpec -> SigSpec -> m ()

prog :: MonadRgb m => m ()
prog = do
  c <- ctr
  r <- c `at` 24
  g <- c `at` 23
  b <- c `at` 22
  rgb r g b

newtype Rgb a = Rgb{ unRgb :: Writer [ModuleBody] a }
  deriving (Functor, Applicative, Monad, MonadWriter [ModuleBody])

instance MonadRgb Rgb where
  ctr = do
    tell $ [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput 1] "\\clk"] <> counter 32 "\\$my_counter" "\\unused" "$my_counter" "$procStmt"
    return $ SigSpecWireId "\\$my_counter"

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

  rgb r g b = do
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\red"
         , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\green"
         , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\blue"
         , ModuleBodyCell $ sbRgbaDrv r g b
         ]

compile :: Rgb a -> File
compile = top . execWriter . unRgb

class MonadProcess m where
  process   :: (SigSpec -> m SigSpec) -> m SigSpec
  increment :: SigSpec -> m SigSpec
  eq        :: SigSpec -> SigSpec -> m SigSpec
  mux       :: SigSpec -> SigSpec -> SigSpec -> m SigSpec

cycleProg :: MonadProcess m => MonadRgb m => m ()
cycleProg = do
  t <- process $ \timer -> do
    t1Sec <- timer `eq` second
    mux t1Sec zero =<< increment timer
  tEqZ <- t `eq` zero
  c <- process $ \color -> do
    cEqBlue <- color `eq` two
    mux tEqZ color =<< mux cEqBlue zero =<< increment color
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

newtype Rtl a = Rtl{ unRtl :: WriterT [ModuleBody] (State Integer) a }
  deriving ( Functor, Applicative, Monad
           , MonadWriter [ModuleBody]
           , MonadState Integer
           )

instance MonadRgb Rtl where
  ctr = do
    tell $ [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput 1] "\\clk"] <> counter 32 "\\$my_counter" "\\unused" "$my_counter" "$procStmt"
    return $ SigSpecWireId "\\$my_counter"

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

  rgb r g b = do
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\red"
         , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\green"
         , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\blue"
         , ModuleBodyCell $ sbRgbaDrv r g b
         ]

instance MonadProcess Rtl where
  process f = do
    i <- get
    modify (+ 1)
    let old = fromString $ "\\ident" <> show i
    j <- get
    modify (+ 1)
    let procStmt = fromString $ "$ident" <> show j
    tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] old]
    srcSig <- f $ SigSpecWireId old
    tell [ModuleBodyProcess $ updateP procStmt
                                (DestSigSpec $ SigSpecWireId old)
                                (SrcSigSpec  $ srcSig)
         ]
    return $ SigSpecWireId old
 
  increment a = do
    i <- get
    modify (+ 1)
    let y = fromString $ "\\ident" <> show i
    j <- get
    modify (+ 1)
    let cId = fromString $ "$ident" <> show j
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] y
         , ModuleBodyCell $ addC cId False 32 False 32 32 a (SigSpecConstant $ ConstantInteger 1) y
         ]
    return $ SigSpecWireId y
  eq a b = do
    i <- get
    modify (+ 1)
    let y = fromString $ "\\ident" <> show i
    j <- get
    modify (+ 1)
    let cId = fromString $ "$ident" <> show j
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 1] y
         , ModuleBodyCell $ eqC cId False 32 False 32 1 a b y
         ]
    return $ SigSpecWireId y
  mux s a b = do
    i <- get
    modify (+ 1)
    let y = fromString $ "\\ident" <> show i
    j <- get
    modify (+ 1)
    let cId = fromString $ "$ident" <> show j
    tell [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] y
         , ModuleBodyCell $ muxC cId 32 a b s y
         ]
    return $ SigSpecWireId y

cycleCompile :: Rtl a -> File
cycleCompile = top . clocked . flip evalState 1 . execWriterT . unRtl

clocked :: [ModuleBody] -> [ModuleBody]
clocked = (ModuleBodyWire (Wire [] $ WireStmt [WireOptionInput 0] "\\clk") :)
