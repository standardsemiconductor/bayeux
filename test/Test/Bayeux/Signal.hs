{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Signal
  ( tests
  ) where

import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Width
import Data.Array
import Data.Finite
import Data.Word
import Prettyprinter
import Prettyprinter.Render.String
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "encoding" valEncoding
  , testGroup "slicing"  sigSlicing
  ]

valEncoding :: [TestTree]
valEncoding =
  [ valTest True  "1'1"
  , valTest False "1'0"
  , valTest (0xFF :: Word8) "8'11111111"
  , valTest (Nothing :: Maybe Bool) "2'00"
  , valTest (Just False) "2'10"
  , valTest (Just True)  "2'11"
  , valTest (0 :: Finite 1) "1'0"
  , valTest (1 :: Finite 2) "1'1"
  , valTest (2 :: Finite 3) "2'10"
  , valTest (1 :: Finite 3) "2'01"
  , valTest (7 :: Finite 8) "3'111"
  , valTest (2 :: Finite 3, 7 :: Finite 8) "5'10111"
  , valTest (Just False, Just True) "4'1011"
  , let a :: Array (Finite 3) (Maybe Bool)
        a = listArray (0, 2) [Just True, Nothing, Just False]
    in valTest a "6'110010"
  , let a :: Maybe (Array (Finite 2) (Maybe Word8))
        a = Just $ listArray (0, 1) $ [Just 0x38, Just 0x02]
    in valTest a "19'1100111000100000010"
  , valTest 'r' "8'01110010"
  , let a :: Maybe (Array (Finite 1) Word8)
        a = Just $ listArray (0, 0) [0xFE]
    in valTest a "9'111111110"
  , valTest (Left False :: Either Bool Bool) "2'00"
  , valTest (Right 0xFE :: Either Bool Word8) "9'111111110"
  ]

valTest :: Encode a => Show a => Width a => a -> Sig a -> TestTree
valTest a s = testCase testName $ val a @?= s
  where
    testName = show a <> " ~ " <> renderPretty s

sigSlicing :: [TestTree]
sigSlicing =
  [ testCase "sliceFst" $ (sliceFst . val) (False, True) @?= slice 1 1 (val (False, True))
  , testCase "sliceSnd" $ (sliceSnd . val) (False, True) @?= slice 0 0 (val (False, True))
  , testCase "sliceValid" $ (sliceValid . val) (Nothing :: Maybe Word8) @?= (slice 8 8 . val) (Nothing :: Maybe Word8)
  , testCase "sliceValue" $ (sliceValue . val) (Just 0x34 :: Maybe Word8) @?= (slice 7 0 . val) (Just 0x34 :: Maybe Word8)
  ]

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty
