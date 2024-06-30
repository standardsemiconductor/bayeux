{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Signal
  ( tests
  ) where

import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Width
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "val encoding" valEncoding
  ]

valEncoding :: [TestTree]
valEncoding =
  [ valTest True  "1'1"
  , valTest False "1'0"
  , valTest (0xFF :: Word8) "8'11111111"
  , valTest (Nothing :: Maybe Bool) "2'00"
  , valTest (Just False) "2'10"
  , valTest (Just True)  "2'11"
  ]

valTest :: Encode a => Width a => a -> Sig a -> TestTree
valTest a s = testCase "" $ val a @?= s
