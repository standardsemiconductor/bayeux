{-# LANGUAGE LambdaCase #-}

module Bayeux (app) where

import Bayeux.Cli
import Bayeux.Flow
import Bayeux.Lp
import Bayeux.RgbCounter
import Bayeux.Rtlil
import Bayeux.Tableaux
import Control.Monad
import Data.Maybe
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Set     as S
import Paths_bayeux
import System.FilePath
import Text.Megaparsec hiding (parse)

app :: Cli -> IO ()
app = \case
  CliDemo demo iceprog -> case demo of
    FiatLux    -> flow iceprog "FiatLux" fiatLux =<< getDataFileName ("data" </> "FiatLux" <.> "pcf")
    RgbCounter -> flow iceprog "RgbCounter" rgbCounter =<< getDataFileName ("data" </> "RgbCounter" <.> "pcf")
    RgbCycle   -> flow iceprog "RgbCycle" rgbCycle =<< getDataFileName ("data" </> "RgbCycle" <.> "pcf")
  CliProve cli -> do
    lp <- fromJust <$> case input cli of
      FileInput f -> parseMaybe (parse <* eof) <$> TIO.readFile f
      StdInput    -> parseMaybe parse <$> TIO.getLine
    let t = unfold $ S.singleton $ Bar lp
    when (tableaux cli) $ putStrLn $ renderTableaux $ T.unpack . render <$> t
    print $ close [] t

rgbCounter :: File
rgbCounter = cycleCompile prog

rgbCycle :: File
rgbCycle = cycleCompile cycleProg
