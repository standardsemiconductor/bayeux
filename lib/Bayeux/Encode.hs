{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bayeux.Encode where

import Bayeux.Width
import Data.Bits
import Data.Bool
import Data.Finite
import Data.String
import Data.Word
import GHC.TypeLits
import Prettyprinter hiding (width)

data BinaryDigit = B0
                 | B1
                 | X
                 | Z
                 | M
                 | D
  deriving (Eq, Read, Show)

instance Pretty BinaryDigit where
  pretty = \case
    B0 -> "0"
    B1 -> "1"
    X  -> "x"
    Z  -> "z"
    M  -> "m"
    D  -> "-"

instance IsString BinaryDigit where
  fromString = \case
    "0" -> B0
    "1" -> B1
    "x" -> X
    "z" -> Z
    "m" -> M
    _   -> D
class Encode a where
  encode :: a -> [BinaryDigit]

instance Encode Bool where
  encode True  = [B1]
  encode False = [B0]

binaryDigits :: Bits b => Width b => b -> [BinaryDigit]
binaryDigits b = bool B0 B1 . testBit b <$> reverse [0..w - 1]
  where
    w = fromIntegral $ width b

instance Encode Word8 where
  encode = binaryDigits

instance Encode Word16 where
  encode = binaryDigits

instance Encode Word32 where
  encode = binaryDigits

instance Encode Word64 where
  encode = binaryDigits

instance KnownNat n => Encode (Finite n) where
  encode b = bool B0 B1 . testBit (getFinite b) <$> reverse [0..w - 1]
    where
      w = fromIntegral $ width b

instance (Encode a, Width a) => Encode (Maybe a) where
  encode = \case
    Nothing -> B0 : replicate w B0
    Just a  -> B1 : encode a
    where
      w = fromIntegral $ width (undefined :: a)
