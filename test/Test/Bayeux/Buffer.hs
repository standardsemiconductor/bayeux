{-# LANGUAGE DataKinds #-}

module Test.Bayeux.Buffer
  ( tests
  ) where

import Bayeux.Buffer
import Bayeux.Width
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "width"
    [ testCase "Cobuf" $ width (undefined :: Cobuf 1 Bool) @?= 4
    ]
  ]
