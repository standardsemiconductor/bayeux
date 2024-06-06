{-# LANGUAGE OverloadedStrings #-}

module Test.Bayeux.Rtlil
  ( tests
  ) where

import Bayeux.Rtlil
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
      [ prettyTest "led"       rtlilLed
      , prettyTest "sbRgbaDrv" sbRgbaDrv
      , prettyTest "fiatLux"   fiatLux
      , prettyTest "add"     $ add "\\adder" False 32 False 32 33 (SigSpecWireId "\\a") (SigSpecWireId "\\b") "\\y"
      ]
  , testGroup "synth"
      [ synthTest "led"     rtlilLed
      , synthTest "fiatLux" fiatLux
      ]
  ]

prettyTest :: Pretty a => TestName -> a -> TestTree
prettyTest n = goldenVsString n (curDir </> n' <.> "golden")
                 . return . fromString . T.unpack . render . pretty
  where
    n' = "pretty-" <> n

synthTest :: TestName -> File -> TestTree
synthTest n rtl = testCase n $ withTempFile $ \t -> do
  TIO.writeFile t $ render $ pretty rtl
  let c = "yosys -q -p \"synth_ice40\" -f rtlil " <> t
  (ExitSuccess @=?) =<< waitForProcess =<< spawnCommand c

curDir :: FilePath
curDir = "test" </> "Test" </> "Bayeux" </> "Rtlil"

rtlilLed :: File
rtlilLed = File Nothing
  [ Module
      []
      "\\top"
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput  1] "\\clk"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\LED_R"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\LED_G"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\LED_B"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 26] "\\counter"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] "\\counter_plus_one"
      , ModuleBodyCell $ Cell
          []
          (CellStmt "$add" "$increment")
          [ CellParameter Nothing "\\A_SIGNED" $ ConstantInteger 0
          , CellParameter Nothing "\\A_WIDTH"  $ ConstantInteger 26
          , CellParameter Nothing "\\B_SIGNED" $ ConstantInteger 0
          , CellParameter Nothing "\\B_WIDTH"  $ ConstantInteger 32
          , CellParameter Nothing "\\Y_WIDTH"  $ ConstantInteger 32
          , CellConnect "\\A" $ SigSpecWireId "\\counter"
          , CellConnect "\\B" $ SigSpecConstant $ ConstantInteger 1
          , CellConnect "\\Y" $ SigSpecWireId "\\counter_plus_one"
          ]
          CellEndStmt
      , ModuleBodyCell $ Cell
          []
          (CellStmt "$not" "$not$1")
          [ CellParameter Nothing "\\A_SIGNED" $ ConstantInteger 0
          , CellParameter Nothing "\\A_WIDTH"  $ ConstantInteger 1
          , CellParameter Nothing "\\Y_WIDTH"  $ ConstantInteger 1
          , CellConnect "\\A" $ SigSpecSlice
              (SigSpecWireId "\\counter")
              23
              Nothing
          , CellConnect "\\Y" $ SigSpecWireId "\\LED_R"
          ]
          CellEndStmt
      , ModuleBodyCell $ Cell
          []
          (CellStmt "$not" "$not$2")
          [ CellParameter Nothing "\\A_SIGNED" $ ConstantInteger 0
          , CellParameter Nothing "\\A_WIDTH"  $ ConstantInteger 1
          , CellParameter Nothing "\\Y_WIDTH"  $ ConstantInteger 1
          , CellConnect "\\A" $ SigSpecSlice
              (SigSpecWireId "\\counter")
              24
              Nothing
          , CellConnect "\\Y" $ SigSpecWireId "\\LED_G"
          ]
          CellEndStmt
      , ModuleBodyCell $ Cell
          []
          (CellStmt "$not" "$not$3")
          [ CellParameter Nothing "\\A_SIGNED" $ ConstantInteger 0
          , CellParameter Nothing "\\A_WIDTH"  $ ConstantInteger 1
          , CellParameter Nothing "\\Y_WIDTH"  $ ConstantInteger 1
          , CellConnect "\\A" $ SigSpecSlice
              (SigSpecWireId "\\counter")
              25
              Nothing
          , CellConnect "\\Y" $ SigSpecWireId "\\LED_B"
          ]
          CellEndStmt
      , ModuleBodyProcess $ Process
          []
          "$run"
          (ProcessBody
             []
             Nothing
             []
             [ Sync
                 (SyncStmt Posedge (SigSpecWireId "\\clk"))
                 [ UpdateStmt
                     (DestSigSpec $ SigSpecWireId "\\counter")
                     (SrcSigSpec $ SigSpecSlice
                        (SigSpecWireId "\\counter_plus_one")
                        25
                        (Just 0)
                     )
                 ]
             ]
          )
          ProcEndStmt
      ]
      ModuleEndStmt
  ] 

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions
