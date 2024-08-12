{-# LANGUAGE DataKinds #-}

module Test.Bayeux.Buffer
  ( tests
  ) where

import Bayeux.Buffer
import Bayeux.Width
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "width"
    [ testCase "Cobuf"     $ width (undefined :: Cobuf 1 Bool)            @?= 4
    , testCase "CobufByte" $ width (undefined :: Cobuf 4 Word8)           @?= 36
    , testCase "CobufBig"  $ width (undefined :: Cobuf 99 (Maybe Word32)) @?= 3276
    ]
  ]
