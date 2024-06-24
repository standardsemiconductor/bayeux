{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators     #-}

module Bayeux.Width where

import Data.Word
import GHC.Generics

class Width1 f where
  width1 :: f p -> Integer

defaultWidth :: (Generic a, Width1 (Rep a)) => a -> Integer
defaultWidth = width1 . from

instance Width1 V1 where
  width1 _ = 0

instance Width1 U1 where
  width1 _ = 1

instance Width c => Width1 (K1 i c) where
  width1 _ = width (undefined :: c)

instance Width1 f => Width1 (M1 i c f) where
  width1 (M1 x) = width1 x

instance (Width1 a, Width1 b) => Width1 (a :+: b) where
  width1 (L1 x) = 1 + width1 x
  width1 (R1 x) = 1 + width1 x

instance (Width1 a, Width1 b) => Width1 (a :*: b) where
  width1 (a :*: b) = width1 a + width1 b

class Width a where
  width :: a -> Integer
  default width :: (Generic a, Width1 (Rep a)) => a -> Integer
  width = defaultWidth

instance Width Bool where
  width _ = 1

instance Width Word8 where
  width _ = 8

instance Width Word16 where
  width _ = 16

instance Width Word32 where
  width _ = 32

instance Width Word64 where
  width _ = 64
