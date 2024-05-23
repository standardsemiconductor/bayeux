module Bayeux (app) where

import Bayeux.Cli
import Bayeux.Lp
import Data.Maybe
import qualified Data.Text.IO as TIO
import Text.Megaparsec hiding (parse)

app :: Cli -> IO ()
app cli = print . prove . fromJust =<< case input cli of
  FileInput f -> parseMaybe (parse <* eof) <$> TIO.readFile f
  StdInput    -> parseMaybe parse <$> TIO.getLine
