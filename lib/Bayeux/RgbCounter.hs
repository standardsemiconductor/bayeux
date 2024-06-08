{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Bayeux.RgbCounter
  ( prog
  , compile
  ) where

import Bayeux.Rtlil
import Control.Monad.Writer

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
    tell $ [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput 1] "\\clk"] <> counter 32 "$my_counter" "\\unused"
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

