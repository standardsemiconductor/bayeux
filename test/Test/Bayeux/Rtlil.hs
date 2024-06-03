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
  [ goldenVsString "pretty-led" (curDir </> "pretty-led" <.> "golden") $ return $
      fromString $ T.unpack $ render $ pretty rtlilLed
  , testCase "synth-led" $ withTempFile $ \f -> do
      TIO.writeFile f $ render $ pretty rtlilLed
      let c = "yosys -q -p \"synth_ice40\" -f rtlil " <> f
      (ExitSuccess @=?) =<< waitForProcess =<< spawnCommand c
  ]

curDir :: FilePath
curDir = "test" </> "Test" </> "Bayeux" </> "Rtlil"

rtlilLed :: File
rtlilLed = File Nothing
  [ Module
      []
      (ModuleStmt $ Ident "\\top")
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput  1] $ WireId $ Ident "\\clk"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] $ WireId $ Ident "\\LED_R"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] $ WireId $ Ident "\\LED_G"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] $ WireId $ Ident "\\LED_B"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 26] $ WireId $ Ident "\\counter"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] $ WireId $ Ident "\\counter_plus_one"
      , ModuleBodyCell $ Cell
          []
          (CellStmt (CellType $ Ident "$add") (CellId $ Ident "$increment"))
          [ CellParameter Nothing (Ident "\\A_SIGNED") $ ConstantInteger 0
          , CellParameter Nothing (Ident "\\A_WIDTH")  $ ConstantInteger 26
          , CellParameter Nothing (Ident "\\B_SIGNED") $ ConstantInteger 0
          , CellParameter Nothing (Ident "\\B_WIDTH")  $ ConstantInteger 32
          , CellParameter Nothing (Ident "\\Y_WIDTH")  $ ConstantInteger 32
          , CellConnect (Ident "\\A") $ SigSpecWireId $ WireId $ Ident "\\counter"
          , CellConnect (Ident "\\B") $ SigSpecConstant $ ConstantInteger 1
          , CellConnect (Ident "\\Y") $ SigSpecWireId $ WireId $ Ident "\\counter_plus_one"
          ]
          CellEndStmt
      , ModuleBodyCell $ Cell
          []
          (CellStmt (CellType $ Ident "$not") (CellId $ Ident "$not$1"))
          [ CellParameter Nothing (Ident "\\A_SIGNED") $ ConstantInteger 0
          , CellParameter Nothing (Ident "\\A_WIDTH")  $ ConstantInteger 1
          , CellParameter Nothing (Ident "\\Y_WIDTH")  $ ConstantInteger 1
          , CellConnect (Ident "\\A") $ SigSpecSlice
              (SigSpecWireId $ WireId $ Ident "\\counter")
              23
              Nothing
          , CellConnect (Ident "\\Y") $ SigSpecWireId $ WireId $ Ident "\\LED_R"
          ]
          CellEndStmt
      , ModuleBodyCell $ Cell
          []
          (CellStmt (CellType $ Ident "$not") (CellId $ Ident "$not$2"))
          [ CellParameter Nothing (Ident "\\A_SIGNED") $ ConstantInteger 0
          , CellParameter Nothing (Ident "\\A_WIDTH")  $ ConstantInteger 1
          , CellParameter Nothing (Ident "\\Y_WIDTH")  $ ConstantInteger 1
          , CellConnect (Ident "\\A") $ SigSpecSlice
              (SigSpecWireId $ WireId $ Ident "\\counter")
              24
              Nothing
          , CellConnect (Ident "\\Y") $ SigSpecWireId $ WireId $ Ident "\\LED_G"
          ]
          CellEndStmt
      , ModuleBodyCell $ Cell
          []
          (CellStmt (CellType $ Ident "$not") (CellId $ Ident "$not$3"))
          [ CellParameter Nothing (Ident "\\A_SIGNED") $ ConstantInteger 0
          , CellParameter Nothing (Ident "\\A_WIDTH")  $ ConstantInteger 1
          , CellParameter Nothing (Ident "\\Y_WIDTH")  $ ConstantInteger 1
          , CellConnect (Ident "\\A") $ SigSpecSlice
              (SigSpecWireId $ WireId $ Ident "\\counter")
              25
              Nothing
          , CellConnect (Ident "\\Y") $ SigSpecWireId $ WireId $ Ident "\\LED_B"
          ]
          CellEndStmt
      , ModuleBodyProcess $ Process
          []
          (ProcStmt $ Ident "$run")
          (ProcessBody
             []
             Nothing
             []
             [ Sync
                 (SyncStmt Posedge (SigSpecWireId $ WireId $ Ident "\\clk"))
                 [ UpdateStmt
                     (DestSigSpec $ SigSpecWireId $ WireId $ Ident "\\counter")
                     (SrcSigSpec $ SigSpecSlice
                        (SigSpecWireId $ WireId $ Ident "\\counter_plus_one")
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
