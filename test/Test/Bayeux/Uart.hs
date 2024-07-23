module Test.Bayeux.Uart
  ( tests
  ) where

import Bayeux.Uart
import Bayeux.Rtl
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prettyprinter
import Prettyprinter.Render.Text
import System.Exit
import System.FilePath
import System.IO.Extra
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

tests :: [TestTree]
tests =
  [ testGroup "pretty"
      [ prettyTest "hello" $ handleErr $ compile hello
      , prettyTest "echo"  $ handleErr $ compile echo
      , prettyTest "spramReverse" $ handleErr $ compile spramReverse
      ]
  , testGroup "synth"
      [ synthTest "hello" $ handleErr $ compile hello
      , synthTest "echo"  $ handleErr $ compile echo
      , synthTest "spramReverse" $ handleErr $ compile spramReverse
      ]
  ]

handleErr :: Either Err File -> File
handleErr = either (error . show) id

prettyTest :: Pretty a => TestName -> a -> TestTree
prettyTest n = goldenVsString n (curDir </> n <.> "pretty")
                 . return . fromString . T.unpack . render . pretty

synthTest :: TestName -> File -> TestTree
synthTest n rtl = testCase n $ withTempFile $ \t -> do
  TIO.writeFile t $ render $ pretty rtl
  let c = "yosys -q -p \"synth_ice40\" -f rtlil " <> t
  (ExitSuccess @=?) =<< waitForProcess =<< spawnCommand c

curDir :: FilePath
curDir = "test" </> "Test" </> "Bayeux" </> "Uart" </> "golden"

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions
