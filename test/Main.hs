module Main (main) where

import qualified Test.Bayeux.Ice40
import qualified Test.Bayeux.Lp
import qualified Test.Bayeux.Rtl
import qualified Test.Bayeux.Signal
import qualified Test.Bayeux.Uart
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Test.Bayeux"
  [ testGroup "Ice40"  Test.Bayeux.Ice40.tests
  , testGroup "Lp"     Test.Bayeux.Lp.tests
  , testGroup "Rtl"    Test.Bayeux.Rtl.tests
  , testGroup "Signal" Test.Bayeux.Signal.tests
  , testGroup "Uart"   Test.Bayeux.Uart.tests
  ]
