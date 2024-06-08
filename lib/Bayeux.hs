module Bayeux (app) where

import Bayeux.Cli
import Bayeux.Lp
import Bayeux.Tableaux
import Control.Monad
import Data.Maybe
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Set     as S
import Text.Megaparsec hiding (parse)

app :: Cli -> IO ()
app cli = do
  lp <- fromJust <$> case input cli of
    FileInput f -> parseMaybe (parse <* eof) <$> TIO.readFile f
    StdInput    -> parseMaybe parse <$> TIO.getLine
  let t = unfold $ S.singleton $ Bar lp
  when (tableaux cli) $ putStrLn $ renderTableaux $ T.unpack . render <$> t
  print $ close [] t
