{-# LANGUAGE DataKinds #-}

module Test.Bayeux.Buffer
  ( tests
  ) where

import Bayeux.Buffer
import Bayeux.Encode
import Bayeux.Width
import Data.Array
import Data.Finite
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import Yosys.Rtl

tests :: [TestTree]
tests =
  [ testGroup "width"
    [ testCase "Cobuf"     $ width (undefined :: Cobuf 1 Bool)            @?= 4
    , testCase "CobufByte" $ width (undefined :: Cobuf 4 Word8)           @?= 36
    , testCase "CobufBig"  $ width (undefined :: Cobuf 99 (Maybe Word32)) @?= 3276
    ]
  , testGroup "encode"
    [ let actual = Cobuf Idle 0 Nothing :: Cobuf 1 Bool
      in testCase "CobufNothing" $ encode actual @?= [B0, B0, B0, B0]

    , let actual = Cobuf Busy 2 $ Just $ listArray (0 :: Finite 4, 3) [0xAB :: Word8, 0xBC, 0xCD, 0xEF]
      in testCase "CobufJust" $ encode actual @?=
           [ B1     -- fsm
           , B1, B0 -- ix
           , B1     -- Maybe
           , B1, B0, B1, B0, B1, B0, B1, B1 -- 0xAB
           , B1, B0, B1, B1, B1, B1, B0, B0 -- 0xBC
           , B1, B1, B0, B0, B1, B1, B0, B1 -- 0xCD
           , B1, B1, B1, B0, B1, B1, B1, B1 -- 0xEF
           ]
    ]
  ]
