{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Width where

import Data.Array
import Data.Finite
import Data.Proxy
import Data.Word
import GHC.TypeLits

class Width a where
  width :: a -> Integer

instance Width Bool where
  width _ = 1

instance Width Word8 where
  width _ = 8

instance Width Char where
  width _ = 8

instance Width Word16 where
  width _ = 16

instance Width Word32 where
  width _ = 32

instance Width Word64 where
  width _ = 64

instance (Width a, Width b) => Width (a, b) where
  width (a, b) = width a + width b

instance Width a => Width (Maybe a) where
  width _ = 1 + width (undefined :: a)

instance KnownNat n => Width (Finite n) where
  width _
    | n == 1    = 1
    | otherwise = ceil $ logBase 2 $ fromInteger n
    where
      n = natVal (Proxy :: Proxy n)
      ceil :: Double -> Integer
      ceil = ceiling

instance (KnownNat n, Width e) => Width (Array (Finite n) e) where
  width _ = (fromIntegral . natVal) (Proxy :: Proxy n) * width (undefined :: e)
