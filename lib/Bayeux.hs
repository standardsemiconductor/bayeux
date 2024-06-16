{-# LANGUAGE LambdaCase #-}

module Bayeux (app) where

import Bayeux.Cli
import Bayeux.Flow
import Bayeux.Lp
import Bayeux.Rgb
import Bayeux.Rtl
import Bayeux.Tableaux
import Bayeux.Uart
import Control.Concurrent.Async
import Control.Monad
import Data.Maybe
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Set     as S
import Paths_bayeux
import System.FilePath
import System.Hardware.Serialport
import System.IO
import Text.Megaparsec hiding (parse)

app :: Cli -> IO ()
app = \case
  CliDemo demo iceprog -> case demo of
    FiatLux    -> flow iceprog "FiatLux" fiatLux =<< getDataFileName ("data" </> "FiatLux" <.> "pcf")
    RgbCounter -> flow iceprog "RgbCounter" rgbCounter =<< getDataFileName ("data" </> "RgbCounter" <.> "pcf")
    RgbCycle   -> flow iceprog "RgbCycle" rgbCycle =<< getDataFileName ("data" </> "RgbCycle" <.> "pcf")
    Hello      -> flow iceprog "Hello" (compile hello) =<< getDataFileName ("data" </> "Hello" <.> "pcf")
  CliProve cli -> do
    lp <- fromJust <$> case input cli of
      FileInput f -> parseMaybe (parse <* eof) <$> TIO.readFile f
      StdInput    -> parseMaybe parse <$> TIO.getLine
    let t = unfold $ S.singleton $ Bar lp
    when (tableaux cli) $ putStrLn $ renderTableaux $ T.unpack . render <$> t
    print $ close [] t
  CliCom -> com "/dev/ttyUSB0"

rgbCounter :: File
rgbCounter = compile prog

rgbCycle :: File
rgbCycle = compile cycleProg

com :: String -> IO ()
com portPath = hWithSerial portPath serialPortSettings $ \hndl -> do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  concurrently_ (readUart hndl) (writeUart hndl)
  where
    readUart  hndl = forever $ putChar =<< hGetChar hndl
    writeUart hndl = forever $ hPutChar hndl =<< getChar

serialPortSettings :: SerialPortSettings
serialPortSettings = defaultSerialSettings{ commSpeed = CS19200 }
