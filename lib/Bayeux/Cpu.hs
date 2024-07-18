{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Cpu
  ( Instr(..)
  , Prog(..)
  , soc
  , prog
  , run
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Uart
import Bayeux.Width
import Control.Concurrent.Async
import Control.Monad
import Data.Array
import Data.Char
import Data.Finite
import Data.Word
import System.Hardware.Serialport
import System.IO

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

run :: Prog -> IO ()
run (Prog instrs) = hWithSerial "/dev/ttyUSB0" serialPortSettings $ \hndl -> do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  concurrently_ (readUart hndl) (writeUart hndl)
  where
    readUart  hndl = forever $ putChar =<< hGetChar hndl
    writeUart hndl = mapM_ (hPutChar hndl) $ toChars =<< instrs
      where
        toChars = \case -- todo, use encode, then binary digits to chars
          Out b -> [chr 1, (chr . fromIntegral) b]
          Halt  -> [chr 0, chr 0]

serialPortSettings :: SerialPortSettings
serialPortSettings = defaultSerialSettings{ commSpeed = CS19200 }
{-
embed :: Prog -> Array (Finite 12) Instr
embed (Prog instrs) = listArray (0, 11) instrs
-}
