{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Signal
  ( tests
  ) where

import Bayeux.Signal
import Data.Finitary
import Data.Finite
import Data.Vector.Sized (Vector, fromTuple)
import qualified Data.Vector.Sized as V
import Data.Word
import Prettyprinter
import Prettyprinter.Render.String
import Test.Tasty
import Test.Tasty.HUnit

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
  , sigTest (Just False) "2'01"
  , sigTest (Just True)  "2'10"
  , sigTest (0 :: Finite 1) "1'0"
  , sigTest (1 :: Finite 2) "1'1"
  , sigTest (2 :: Finite 3) "2'10"
  , sigTest (1 :: Finite 3) "2'01"
  , sigTest (7 :: Finite 8) "3'111"
  , sigTest (2 :: Finite 3, 7 :: Finite 8) "5'10111"
  , sigTest (Just False, Just True) "4'0101"
  , let a :: Vector 3 (Maybe Bool)
        a = fromTuple (Just True, Nothing, Just False)
    in sigTest a "6'110010"
  , let a :: Maybe (Vector 2 (Maybe Word8))
        a = Just $ fromTuple (Just 0x38, Just 0x02)
    in sigTest a "19'1100111000100000010"
  , sigTest 'r' "8'01110010"
  , let a :: Maybe (Vector 1 Word8)
        a = Just $ V.singleton 0xFE
    in sigTest a "9'111111110"
  , sigTest (Left False :: Either Bool Bool) "2'00"
  , sigTest (Right 0xFE :: Either Bool Word8) "9'111111110"
  ]

sigTest :: Finitary a => Show a => a -> Sig a -> TestTree
sigTest a s = testCase testName $ sig a @?= s
  where
    testName = show a <> " ~ " <> renderPretty s

sigSlicing :: [TestTree]
sigSlicing =
  [ testCase "sliceFst" $ (sliceFst . sig) (False, True) @?= slice 1 1 (sig (False, True))
  , testCase "sliceSnd" $ (sliceSnd . sig) (False, True) @?= slice 0 0 (sig (False, True))
  , testCase "sliceValid" $ (sliceValid . sig) (Nothing :: Maybe Word8) @?= (slice 8 8 . sig) (Nothing :: Maybe Word8)
  , testCase "sliceValue" $ (sliceValue . sig) (Just 0x34 :: Maybe Word8) @?= (slice 7 0 . sig) (Just 0x34 :: Maybe Word8)
  ]

renderPretty :: Pretty a => a -> String
renderPretty = renderString . layoutPretty defaultLayoutOptions . pretty
