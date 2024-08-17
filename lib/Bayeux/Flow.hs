module Bayeux.Flow
  ( flow
  ) where

import Development.Shake
import Development.Shake.FilePath
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Prettyprinter
import Prettyprinter.Render.Text
import Yosys.Rtl

buildDir :: FilePath -> FilePath
buildDir = combine "_build"

flow :: Bool -> Bool -> String -> File -> FilePath -> IO ()
flow prog clean name designFile pcfFile = shake shakeOptions{ shakeFiles = buildDir name } $ do

  want $ if clean
    then ["clean"]
    else if prog
    then ["prog"]
    else [buildDir name </> "pack.bin"]

  phony "clean" $ do
    putInfo $ "Cleaning files in " <> buildDir name
    removeFilesAfter (buildDir name) ["//*"]

  phony "compile" $ need [buildDir name </> "compile.rtlil"]
  phony "synth"   $ need [buildDir name </> "synth.json"]
  phony "pnr"     $ need [buildDir name </> "pnr.asc"]
  phony "pack"    $ need [buildDir name </> "pack.bin"]
  phony "prog"    $ do
    putInfo "Program VELDT"
    need [buildDir name </> "pack.bin"]
    cmd_ "iceprog" (buildDir name </> "pack.bin")

  -- compile
  buildDir name </> "compile.rtlil" %> \out -> do
    putInfo "compile"
    liftIO $ TIO.writeFile out $ render $ pretty designFile

  -- yosys synthesis
  buildDir name </> "synth.json" %> \out -> do
    putInfo "Synthesizing"
    need [buildDir name </> "compile.rtlil"]
    cmd_ "yosys"
         "-q"
         "-p"
         ["synth_ice40 -json " ++ out]
         "-f rtlil"
         (buildDir name </> "compile.rtlil")

  -- place and route NextPNR
  buildDir name </> "pnr.asc" %> \out -> do
    putInfo "Place and Route"
    need [buildDir name </> "synth.json", pcfFile]
    cmd_ "nextpnr-ice40"
         "--up5k"
         "--package sg48"
         ("--pcf " <> pcfFile)
         "--asc"
         [out]
         ("--json " <> buildDir name </> "synth.json")

  -- ice pack
  buildDir name </> "pack.bin" %> \out -> do
    putInfo "Ice pack"
    need [buildDir name </> "pnr.asc"]
    cmd_ "icepack" (buildDir name </> "pnr.asc") [out]

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions
