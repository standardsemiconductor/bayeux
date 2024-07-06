module Main where

import Bayeux
import Bayeux.Cli
import Data.Foldable
import Options.Applicative

main :: IO ()
main = app =<< execParser opts

opts :: ParserInfo Cli
opts = info (parseCli <**> helper) $ mconcat
  [ fullDesc
  , header "Bayeux"
  ]

parseCli :: Parser Cli
parseCli = asum
  [ CliDemo  <$> parseCmd <*> parseDemo
  , CliProve <$> parseProve
  , parseCliCom
  ]

parseCmd :: Parser Cmd
parseCmd = asum
  [ flag' Prog  $ long "program"    <> short 'p' <> help "Program VELDT FPGA"
  , flag' Synth $ long "synthesize" <> short 's' <> help "Synthesize demo"
  , flag' Clean $ long "clean"      <> short 'c' <> help "Clean demo"
  ]

parseDemo :: Parser Demo
parseDemo = asum
  [ flag' FiatLux $ long "FiatLux" <> help "FiatLux demo"
  , flag' RgbCounter $ long "RgbCounter" <> help "RgbCounter demo"
  , flag' RgbCycle $ long "RgbCycle" <> help "RgbCycle demo"
  , flag' Hello $ long "Hello" <> help "Hello demo"
  , flag' Echo $ long "Echo" <> help "Echo demo"
  , flag' LedCtrl $ long "LedCtrl" <> help "Control Led IP through UART"
  , flag' BufEcho $ long "BufEcho" <> help "BufEcho demo"
  ]

parseProve :: Parser Prove
parseProve = Prove <$> parseInput <*> parseTableauxOutput

parseInput :: Parser Input
parseInput = parseFileInput <|> parseStdInput

parseFileInput :: Parser Input
parseFileInput = FileInput <$> (strOption . mconcat)
  [ long "file"
  , short 'f'
  , metavar "FILENAME"
  , help "Input file"
  ]

parseStdInput :: Parser Input
parseStdInput = flag' StdInput $ long "stdin" <> help "Read from stdin"

parseTableauxOutput :: Parser Bool
parseTableauxOutput = switch $ long "tableaux" <> short 't' <> help "Display tableaux"

parseCliCom :: Parser Cli
parseCliCom = flag' CliCom $ long "com" <> help "Serial com"
