{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Ice40.Rgb
  ( tests
  ) where

import Bayeux.Ice40.Rgb
import Bayeux.Rtl
import Prettyprinter
import System.FilePath
import Test.Bayeux.Rtl (prettyTest, synthTest)
import Test.Tasty
import Yosys.Rtl

tests :: [TestTree]
tests =
  [ testGroup "pretty"
      [ prettyTest' "sbRgbaDrv" $ sbRgbaDrv
          (SigSpecWireId "\\pwm_r")
          (SigSpecWireId "\\pwm_g")
          (SigSpecWireId "\\pwm_b")
          (SigSpecWireId "\\red")
          (SigSpecWireId "\\green")
          (SigSpecWireId "\\blue")
      , prettyTest' "fiatLux"   fiatLux
      , prettyTest' "rgbcounter" $ handleErr $ compile prog
      , prettyTest' "rgbcycle"   $ handleErr $ compile cycleProg
      ]
  , testGroup "synth"
      [ synthTest "fiatLux" fiatLux
      , synthTest "rgbcounter" $ handleErr $ compile prog
      , synthTest "rgbcycle"   $ handleErr $ compile cycleProg
      ]
  ]

handleErr :: Either Err File -> File
handleErr = either (error . show) id

prettyTest' :: Pretty a => TestName -> a -> TestTree
prettyTest' = prettyTest $ "test" </> "Test" </> "Bayeux" </> "Ice40" </> "Rgb" </> "golden"
