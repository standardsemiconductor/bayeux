module Bayeux.Flow
  ( flow
  ) where

import Bayeux.Rtl
import Development.Shake
import Development.Shake.FilePath
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Prettyprinter
import Prettyprinter.Render.Text

flow :: Bool -> Bool -> String -> File -> FilePath -> IO ()
flow prog clean name designFile pcfFile = shake shakeOptions{ shakeFiles = "_build" </> name } $ do

  want $ if clean
    then ["clean"]
    else if prog
    then ["prog"]
    else ["_build" </> name </> "pack.bin"]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter ("_build" </> name) ["//*"]

  phony "compile" $ need ["_build" </> name </> "compile.rtlil"]
  phony "synth"   $ need ["_build" </> name </> "synth.json"]
  phony "pnr"     $ need ["_build" </> name </> "pnr.asc"]
  phony "pack"    $ need ["_build" </> name </> "pack.bin"]
  phony "prog"    $ do
    putInfo "Program VELDT"
    need ["_build" </> name </> "pack.bin"]
    cmd_ "iceprog" ("_build" </> name </> "pack.bin")

  -- compile
  "_build" </> name </> "compile.rtlil" %> \out -> do
    putInfo "compile"
    liftIO $ TIO.writeFile out $ render $ pretty designFile

  -- yosys synthesis
  "_build" </> name </> "synth.json" %> \out -> do
    putInfo "Synthesizing"
    need ["_build" </> name </> "compile.rtlil"]
    cmd_ "yosys"
         "-q"
         "-p"
         ["synth_ice40 -json " ++ out]
         "-f rtlil"
         ("_build" </> name </> "compile.rtlil")

  -- place and route NextPNR
  "_build" </> name </> "pnr.asc" %> \out -> do
    putInfo "Place and Route"
    need ["_build" </> name </> "synth.json", pcfFile {-"app/FiatLux/FiatLux.pcf"-}]
    cmd_ "nextpnr-ice40"
         "--up5k"
         "--package sg48"
         ("--pcf " <> pcfFile)
         "--asc"
         [out]
         ("--json " <> "_build" </> name </> "synth.json")

  -- ice pack
  "_build" </> name </> "pack.bin" %> \out -> do
    putInfo "Ice pack"
    need ["_build" </> name </> "pnr.asc"]
    cmd_ "icepack" ("_build" </> name </> "pnr.asc") [out]

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions
