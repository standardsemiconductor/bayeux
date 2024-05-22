module Bayeux (app) where

import Bayeux.Cli
import Bayeux.Lp
import Data.Maybe
import qualified Data.Text.IO as TIO
import Text.Megaparsec

app :: Cli -> IO ()
app cli = print . proveLp . fromJust =<< case input cli of
  FileInput f -> parseMaybe (parseLp <* eof) <$> TIO.readFile f
  StdInput    -> parseMaybe parseLp <$> TIO.getLine
