module Main where

import Bayeux
import Bayeux.Cli
import Options.Applicative

main :: IO ()
main = app =<< execParser opts

opts :: ParserInfo Cli
opts = info (parseCli <**> helper) $ mconcat
  [ fullDesc
  , header "Bayeux"
  ]

parseCli :: Parser Cli
parseCli = Cli <$> parseInput <*> parseTableauxOutput

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
