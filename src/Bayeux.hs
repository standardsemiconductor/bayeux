{-# LANGUAGE LambdaCase #-}

module Bayeux
  ( Lp(..)
  , (/\), (\/), (==>)
  , app
  , evalLp
  , prettyLp
  , parseLp
  , prove
  , Input(..)
  , Cli(..)
  ) where

import Bayeux.Cli
import Bayeux.Lp
import Data.List.NonEmpty
import Data.Maybe
import qualified Data.Text.IO as TIO
import Text.Megaparsec

app :: Cli -> IO ()
app cli = print . prove . fromJust =<< case input cli of
  FileInput f -> parseMaybe (parseLp <* eof) <$> TIO.readFile f
  StdInput    -> parseMaybe parseLp <$> TIO.getLine

data Tableaux a = Leaf   a
                | Stem   a (Tableaux a)
                | Branch a (Tableaux a) (Tableaux a)
  deriving (Eq, Read, Show)

growLp
  :: NonEmpty (Lp a) -- ^ Assumptions
  -> Tableaux (Lp a)
growLp (e :| es) =  case e of
  Bv{}           -> case nonEmpty es of
    Nothing      -> Leaf e
    Just es'     -> Stem e $ growLp es'
  Bar Bv{}       -> case nonEmpty es of
    Nothing      -> Leaf e
    Just es'     -> Stem e $ growLp es'
  Bar (Bar x)    -> Stem e $ growLp $ x :| es
  Conj x y       -> Stem e $ growLp $ x <| y :| es
  Bar (Disj x y) -> Stem e $ growLp $ Bar x <| Bar y :| es
  Bar (Impl x y) -> Stem e $ growLp $ x <| Bar y :| es
  Bar (Conj x y) ->
    let l = Bar x :| es
        r = Bar y :| es
    in Branch e (growLp l) (growLp r)
  Disj x y ->
    let l = x :| es
        r = y :| es
    in Branch e (growLp l) (growLp r)
  Impl x y ->
    let l = Bar x :| es
        r =     y :| es
    in Branch e (growLp l) (growLp r)

closeLp :: Eq a => [Lp a] -> Tableaux (Lp a) -> Bool
closeLp signedBvs = \case
  Leaf a -> hasContra $ a : signedBvs
  Stem a t
    | isSignedBv a -> closeLp (a : signedBvs) t
    | otherwise    -> closeLp signedBvs t
  Branch a l r
    | isSignedBv a -> let signedBvs' = a : signedBvs
                      in closeLp signedBvs' l &&
                         closeLp signedBvs' r
    | otherwise    -> closeLp signedBvs l &&
                      closeLp signedBvs r
  where
    hasContra :: Eq a => [Lp a] -> Bool
    hasContra bvs = any (flip elem bvs . invert) bvs
      where
        invert :: Lp a -> Lp a
        invert = \case
          Bar v@Bv{} -> v
          v@Bv{}     -> Bar v
          lp         -> lp

prove :: Eq a => Lp a -> Bool
prove = closeLp mempty . growLp . pure . Bar
