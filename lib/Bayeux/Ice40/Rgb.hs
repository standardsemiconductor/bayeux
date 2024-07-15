{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Rgb
  ( sbRgbaDrv
  , fiatLux
  , MonadRgb(..)
  , outputRgb
  , prog
  , cycleProg
  ) where

import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl hiding (at, binary, process, mux, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad.Writer
import Data.Word

sbRgbaDrv
  :: SigSpec -- ^ red   pwm input
  -> SigSpec -- ^ green pwm input
  -> SigSpec -- ^ blue  pwm input
  -> SigSpec -- ^ red   RGB0 output
  -> SigSpec -- ^ green RGB1 output
  -> SigSpec -- ^ blue  RGB2 output
  -> Cell
sbRgbaDrv pwmR pwmG pwmB red green blue = Cell
  [AttrStmt "\\module_not_derived" $ ConstantInteger 1]
  (CellStmt "\\SB_RGBA_DRV" "\\RGBA_DRIVER")
  [ CellParameter Nothing "\\CURRENT_MODE" $ ConstantString "0b1"
  , CellParameter Nothing "\\RGB0_CURRENT" $ ConstantString "0b111111"
  , CellParameter Nothing "\\RGB1_CURRENT" $ ConstantString "0b111111"
  , CellParameter Nothing "\\RGB2_CURRENT" $ ConstantString "0b111111"
  , CellConnect "\\CURREN" $ SigSpecConstant $ ConstantValue $ Value 1 [B1]
  , CellConnect "\\RGB0" red
  , CellConnect "\\RGB0PWM" pwmR
  , CellConnect "\\RGB1" green
  , CellConnect "\\RGB1PWM" pwmG
  , CellConnect "\\RGB2" blue
  , CellConnect "\\RGB2PWM" pwmB
  , CellConnect "\\RGBLEDEN" $ SigSpecConstant $ ConstantValue $ Value 1 [B1]
  ]
  CellEndStmt

fiatLux :: File
fiatLux = top $
  [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 1] "\\red"
  , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\green"
  , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\blue"
  ] <> initial "\\pwm_r" True
    <> initial "\\pwm_g" False
    <> initial "\\pwm_b" False
    <> [ModuleBodyCell $ sbRgbaDrv
          (SigSpecWireId "\\pwm_r")
          (SigSpecWireId "\\pwm_g")
          (SigSpecWireId "\\pwm_b")
          (SigSpecWireId "\\red")
          (SigSpecWireId "\\green")
          (SigSpecWireId "\\blue")
       ]

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
    pats timer
      [ 12000000 ~~> sig (0 :: Word32)
      , wilds t'
      ]
  c <- process $ \color -> do
    c' <- inc color
    c'' <- pats color
      [ Blue ~~> sig Red
      , wilds c'
      ]
    pats t
      [ 12000000 ~~> c''
      , wilds color
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
