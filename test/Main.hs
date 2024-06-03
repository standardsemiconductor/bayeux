module Main (main) where

import qualified Test.Bayeux.Lp
import qualified Test.Bayeux.Rtlil
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Test.Bayeux"
  [ testGroup "Lp"    Test.Bayeux.Lp.tests
  , testGroup "Rtlil" Test.Bayeux.Rtlil.tests
  ]
