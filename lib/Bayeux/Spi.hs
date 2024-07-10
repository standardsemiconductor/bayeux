{-# LANGUAGE LambdaCase #-}

module Bayeux.Spi
  ( MonadSpi(..)
  , isW
  , sliceAddress
  , sliceData
  ) where

import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Width
import Data.Text (Text)
import Data.Word

data Addr = Cr0
          | Cr1
          | Cr2
          | Br
          | Txdr
          | Rxdr
          | Csr
          | Sr
          | Irq
          | Irqen
  deriving (Eq, Read, Show)

instance Encode Addr where
  encode = \case
    Cr0   -> [B1, B0, B0, B0]
    Cr1   -> [B1, B0, B0, B1]
    Cr2   -> [B1, B0, B1, B0]
    Br    -> [B1, B0, B1, B1]
    Txdr  -> [B1, B1, B0, B1]
    Rxdr  -> [B1, B1, B1, B0]
    Csr   -> [B1, B1, B1, B1]
    Sr    -> [B1, B1, B0, B0]
    Irq   -> [B0, B1, B1, B0]
    Irqen -> [B0, B1, B1, B1]

instance Width Addr where
  width _ = 4

-- | Request
data Req = R Addr
         | W Addr
             Word8 -- ^ data

instance Encode Req where
  encode = \case
    R a   -> [B0] <> encode a <> replicate 8 B0
    W a d -> [B1] <> encode a <> encode d

instance Width Req where
  width _ = 13

isW :: Sig Req -> Sig Bool
isW = slice 12 12

sliceAddress :: Sig Req -> Sig Word8
sliceAddress = slice 11 8

sliceData :: Sig Req -> Sig Word8
sliceData = slice 7 0

class MonadSpi m where
  spi :: Text -> Sig (Maybe Req) -> m (Sig (Maybe Word8))
