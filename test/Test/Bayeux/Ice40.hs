module Test.Bayeux.Ice40
  ( tests
  ) where

import qualified Test.Bayeux.Ice40.Led   as Led
import qualified Test.Bayeux.Ice40.Rgb   as Rgb
import qualified Test.Bayeux.Ice40.Spram as Spram
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup "Led"   Led.tests
  , testGroup "Rgb"   Rgb.tests
  , testGroup "Spram" Spram.tests
  ]
