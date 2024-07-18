{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Cpu
  ( Instr(..)
  , Prog(..)
  , soc
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Uart
import Bayeux.Width
import Data.Array
import Data.Char
import Data.Finite
import Data.Word

data Instr = Out Word8
           | Halt
  deriving (Eq, Read, Show)

instance Width Instr where
  width _ = 16

instance Encode Instr where
  encode = \case
    Out b -> replicate 8 B0         <> encode b
    Halt  -> replicate 7 B0 <> [B1] <> replicate 8 B0

sliceOpcode :: Sig Instr -> Sig Word8
sliceOpcode = slice 15 8

sliceOutWord8 :: Sig Instr -> Sig Word8
sliceOutWord8 = slice 7 0

newtype Prog = Prog [Instr]
  deriving (Eq, Read, Show)

cpu :: Monad m => MonadSignal m => Sig (Maybe Instr) -> m (Sig (Maybe Word8))
cpu instrM = ifm
  [ (pure . sliceValid) instrM `thenm` execute (sliceValue instrM)
  , elsem $ val Nothing
  ]
  where
    execute :: Monad m => MonadSignal m => Sig Instr -> m (Sig (Maybe Word8))
    execute instr = patm (sliceOpcode instr)
      [ 0 ~> (pure . justSig . sliceOutWord8) instr
      , wildm $ val Nothing
      ]

justSig :: Sig a -> Sig (Maybe a)
justSig s = Sig $ (spec . sig) True <> spec s

soc :: Monad m => MonadBuffer m => MonadSignal m => MonadUart m => m ()
soc = transmit 624 =<< cpu =<< fmap cast . buffer =<< receive 624 =<< input "\\rx"
  where 
    cast :: Sig (Maybe (Array (Finite 2) Word8)) -> Sig (Maybe Instr)
    cast = Sig . spec

prog :: Prog
prog = Prog $ Out . fromIntegral . ord <$> "Hello World!"
{-
embed :: Prog -> Array (Finite 12) Instr
embed (Prog instrs) = listArray (0, 11) instrs
-}
