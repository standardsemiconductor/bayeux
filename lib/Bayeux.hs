{-# LANGUAGE LambdaCase #-}

module Bayeux (app) where

import Bayeux.Cli
import qualified Bayeux.Cpu as Cpu
import Bayeux.Flow
import Bayeux.Ice40.Led
import Bayeux.Ice40.Rgb
import Bayeux.Lp
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
  CliDemo cmd demo -> case cmd of
    Prog  -> runDemo True  False demo
    Synth -> runDemo False False demo
    Clean -> runDemo False True  demo
    Run   -> Cpu.run Cpu.prog
  CliProve cli -> do
    lp <- fromJust <$> case input cli of
      FileInput f -> parseMaybe (parse <* eof) <$> TIO.readFile f
      StdInput    -> parseMaybe parse <$> TIO.getLine
    let t = unfold $ S.singleton $ Bar lp
    when (tableaux cli) $ putStrLn $ renderTableaux $ T.unpack . render <$> t
    print $ close [] t
  CliCom -> com "/dev/ttyUSB0"
  where
    runDemo iceprog clean d = do
      pcfFile <- getDataFileName $ "data" </> show d <.> "pcf"
      flow iceprog clean (show d) (getDemo d) pcfFile

getDemo :: Demo -> File
getDemo = \case
  FiatLux    -> fiatLux
  RgbCounter -> rgbCounter
  RgbCycle   -> rgbCycle
  Hello      -> handleErr $ compile hello
  Echo       -> handleErr $ compile echo
  LedCtrl    -> handleErr $ compile ledCtrl
  BufEcho    -> handleErr $ compile bufEcho
  Soc        -> handleErr $ compile Cpu.soc

rgbCounter :: File
rgbCounter = handleErr $ compile prog

rgbCycle :: File
rgbCycle = handleErr $ compile cycleProg

handleErr :: Either Err File -> File
handleErr = either (error . show) id

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
