{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Signal
  ( tests
  ) where

import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Width
import Data.Array
import Data.Char
import Data.Finite
import Data.Word
import Prettyprinter
import Prettyprinter.Render.String
import Test.Tasty
import Test.Tasty.HUnit
import Yosys.Rtl

tests :: [TestTree]
tests =
  [ testGroup "encoding" sigEncoding
  , testGroup "slicing"  sigSlicing
  ]

sigEncoding :: [TestTree]
sigEncoding =
  [ sigTest True  "1'1"
  , sigTest False "1'0"
  , sigTest (0xFF :: Word8) "8'11111111"
  , sigTest (Nothing :: Maybe Bool) "2'00"
  , sigTest (Just False) "2'10"
  , sigTest (Just True)  "2'11"
  , sigTest (0 :: Finite 1) "1'0"
  , sigTest (1 :: Finite 2) "1'1"
  , sigTest (2 :: Finite 3) "2'10"
  , sigTest (1 :: Finite 3) "2'01"
  , sigTest (7 :: Finite 8) "3'111"
  , sigTest (2 :: Finite 3, 7 :: Finite 8) "5'10111"
  , sigTest (Just False, Just True) "4'1011"
  , let a :: Array (Finite 3) (Maybe Bool)
        a = listArray (0, 2) [Just True, Nothing, Just False]
    in sigTest a "6'110010"
  , let a :: Maybe (Array (Finite 2) (Maybe Word8))
        a = Just $ listArray (0, 1) [Just 0x38, Just 0x02]
    in sigTest a "19'1100111000100000010"
  , let w8 :: Char -> Word8
        w8 = fromIntegral . ord
    in sigTest (w8 'r') "8'01110010"
  , let a :: Maybe (Array (Finite 1) Word8)
        a = Just $ listArray (0, 0) [0xFE]
    in sigTest a "9'111111110"
  , sigTest (Left False :: Either Bool Bool) "2'00"
  , sigTest (Right 0xFE :: Either Bool Word8) "9'111111110"
  ]

sigTest :: Encode a => Show a => Width a => a -> Sig a -> TestTree
sigTest a s = testCase testName $ sig a @?= s
  where
    testName = show a <> " ~ " <> renderPretty s

sigSlicing :: [TestTree]
sigSlicing =
  [ testCase "sliceFst" $ (sliceFst . sig) (False, True) @?= slice 1 1 (sig (False, True))
  , testCase "sliceSnd" $ (sliceSnd . sig) (False, True) @?= slice 0 0 (sig (False, True))
  , testCase "sliceValid" $ (sliceValid . sig) (Nothing :: Maybe Word8) @?= (slice 8 8 . sig) (Nothing :: Maybe Word8)
  , testCase "sliceValue" $ (sliceValue . sig) (Just 0x34 :: Maybe Word8) @?= (slice 7 0 . sig) (Just 0x34 :: Maybe Word8)
  , testCase "sliceIx0" $ sliceIx 0 byteArrSig @?= slice 7  0  byteArrSig
  , testCase "sliceIx1" $ sliceIx 1 byteArrSig @?= slice 15 8  byteArrSig
  , testCase "sliceIx2" $ sliceIx 2 byteArrSig @?= slice 23 16 byteArrSig
  , testCase "sliceIx0Bool" $ sliceIx 0 boolArrSig @?= slice 0 0 boolArrSig
  , testCase "sliceIx1Bool" $ sliceIx 1 boolArrSig @?= slice 1 1 boolArrSig
  , let expected = SigSpecCat
          [ SigSpecSlice "2'10" 1 $ Just 1
          , SigSpecSlice "2'10" 0 $ Just 0
          ]
    in testCase "sliceRotateBool0" $ sliceRotate 0 boolArrSig @?= Sig expected
  ]
  where
    byteArrSig :: Sig (Array (Finite 3) Word8)
    byteArrSig = sig $ listArray (0, 2) [2, 1, 0]
    boolArrSig :: Sig (Array (Finite 2) Bool)
    boolArrSig = sig $ listArray (0, 1) [True, False]

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty
