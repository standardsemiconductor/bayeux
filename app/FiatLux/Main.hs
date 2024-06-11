module Main where

import Bayeux.RgbCounter
import Bayeux.Rtlil
import Development.Shake
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Prettyprinter
import Prettyprinter.Render.Text

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = "_build" } $ do

  want ["_build/FiatLux.bin"]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  phony "compile" $ need ["_build/FiatLux.rtlil"]
  phony "synth"   $ need ["_build/FiatLux.json"]
  phony "pnr"     $ need ["_build/FiatLux.asc"]
  phony "pack"    $ need ["_build/FiatLux.bin"]
  phony "prog"    $ do
    putInfo "Program VELDT"
    need ["_build/FiatLux.bin"]
    cmd_ "iceprog" "_build/FiatLux.bin"

  -- compile
  "_build/FiatLux.rtlil" %> \out -> do
    putInfo "compile FiatLux"
    liftIO $ TIO.writeFile out $ render $ pretty fiatLux

  -- yosys synthesis
  "_build/FiatLux.json" %> \out -> do
    putInfo "Synthesizing"
    need ["_build/FiatLux.rtlil"]
    cmd_ "yosys"
         "-q"
         "-p"
         ["synth_ice40 -json " ++ out]
         "-f rtlil"
         "_build/FiatLux.rtlil"

  -- place and route NextPNR
  "_build/FiatLux.asc" %> \out -> do
    putInfo "Place and Route"
    need ["_build/FiatLux.json", "app/FiatLux/FiatLux.pcf"]
    cmd_ "nextpnr-ice40"
         "--up5k"
         "--package sg48"
         "--pcf app/FiatLux/FiatLux.pcf"
         "--asc"
         [out]
         "--json _build/FiatLux.json"

  -- ice pack
  "_build/FiatLux.bin" %> \out -> do
    putInfo "Ice pack"
    need ["_build/FiatLux.asc"]
    cmd_ "icepack" "_build/FiatLux.asc" [out]

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions
