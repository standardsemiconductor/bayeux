{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Encode where

import Bayeux.Rtl
import Bayeux.Width
import Data.Word

class Encode a where
  encode :: a -> [BinaryDigit]

instance Encode Bool where
  encode True  = [B1]
  encode False = [B0]

instance Encode Word8 where
  encode = binaryDigits

instance Encode Word16 where
  encode = binaryDigits

instance Encode Word32 where
  encode = binaryDigits

instance Encode Word64 where
  encode = binaryDigits

instance (Encode a, Width a) => Encode (Maybe a) where
  encode = \case
    Nothing -> B0 : replicate w B0
    Just a  -> B1 : encode a
    where
      w = fromIntegral $ width (undefined :: a)
