{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Ice40.Spram
  ( tests
  ) where

import Bayeux.Ice40.Spram
import Bayeux.Rtl
import Bayeux.Signal
import Prettyprinter
import System.FilePath
import Test.Bayeux.Rtl (prettyTest)
import Test.Tasty
import Yosys.Rtl

tests :: [TestTree]
tests =
  [ testGroup "pretty"
      [ prettyTest' "spram" $ handleErr $ compile $ spram
        (Sig "14'00000000000000")
        (Sig "16'0000000000000000")
        (Sig "4'0000")
        (sig False)
        (sig False)
        (sig False)
        (sig False)
        (sig False)
      ]
  ]

handleErr :: Either Err File -> File
handleErr = either (error . show) id

prettyTest' :: Pretty a => TestName -> a -> TestTree
prettyTest' = prettyTest $ "test" </> "Test" </> "Bayeux" </> "Ice40" </> "Spram" </> "golden"
