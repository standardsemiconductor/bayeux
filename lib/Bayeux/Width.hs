{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Width where

import Data.Word

class Width a where
  width :: a -> Integer

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

instance Width a => Width (Maybe a) where
  width _ = 1 + width (undefined :: a)
