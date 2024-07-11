module Test.Bayeux.Ice40.Led
  ( tests
  ) where

import Bayeux.Ice40.Led
import Bayeux.Rtl
import Prettyprinter
import System.FilePath
import Test.Bayeux.Rtl (prettyTest, synthTest)
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup "pretty"
      [ prettyTest' "ledCtrl" $ handleErr $ compile ledCtrl
      ]
  , testGroup "synth"
      [ synthTest "ledCtrl" $ handleErr $ compile ledCtrl
      ]
  ]

handleErr :: Either Err File -> File
handleErr = either (error . show) id

prettyTest' :: Pretty a => TestName -> a -> TestTree
prettyTest' = prettyTest $ "test" </> "Test" </> "Bayeux" </> "Ice40" </> "Led" </> "golden"
