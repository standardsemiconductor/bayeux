module Main (main) where

import qualified Test.Bayeux.Lp
import qualified Test.Bayeux.Rgb
import qualified Test.Bayeux.Rtlil
import qualified Test.Bayeux.Uart
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Test.Bayeux"
  [ testGroup "Lp"    Test.Bayeux.Lp.tests
  , testGroup "Rgb"   Test.Bayeux.Rgb.tests
  , testGroup "Rtlil" Test.Bayeux.Rtlil.tests
  , testGroup "Uart"  Test.Bayeux.Uart.tests
  ]
