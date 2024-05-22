{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Language of propositions
module Bayeux.Lp
  ( Lp(..)
  , (/\), (\/), (==>)
  , isSignedBv
  , evalLp
  , prettyLp
  , parseLp
  , growLp
  , closeLp
  , proveLp
  ) where

import Bayeux.Tableaux
import Control.Monad.Combinators.Expr
import Data.List.NonEmpty
import Data.String
import Data.Text (Text)
import Data.Void
import Prettyprinter hiding (parens)
import Prettyprinter.Render.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Lp a = Bv a
          | Bar  (Lp a)
          | Conj (Lp a) (Lp a)
          | Disj (Lp a) (Lp a)
          | Impl (Lp a) (Lp a)
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

infixr 3 /\
(/\) :: Lp a -> Lp a -> Lp a
(/\) = Conj

infixr 2 \/
(\/) :: Lp a -> Lp a -> Lp a
(\/) = Disj

infixr 1 ==>
(==>) :: Lp a -> Lp a -> Lp a
(==>) = Impl

instance IsString a => IsString (Lp a) where
  fromString = \case
    '~' : s -> Bar $ Bv $ fromString s
    s       -> Bv $ fromString s

isSignedBv :: Lp a -> Bool
isSignedBv = \case
  Bar Bv{} -> True
  Bv{}     -> True
  _        -> False

evalLp :: Lp Bool -> Bool
evalLp = \case
  Bv b     -> b
  Bar  x   -> not $ evalLp x
  Conj x y -> evalLp x && evalLp y
  Disj x y -> evalLp x || evalLp y
  Impl x y -> not (evalLp x) || evalLp y

instance Pretty a => Pretty (Lp a) where
  pretty = \case
    Bv b     -> pretty b
    Bar x    -> "~" <> prettySub x
    Conj x y -> prettySub x <+> "/\\" <+> prettySub y
    Disj x y -> prettySub x <+> "\\/" <+> prettySub y
    Impl x y -> prettySub x <+> "=>"  <+> prettySub y
    where
      prettySub = \case
        e@Bv{} -> pretty e
        e      -> "(" <> pretty e <> ")"

prettyLp :: Pretty a => Lp a -> Text
prettyLp = renderStrict . layoutPretty defaultLayoutOptions . pretty

------------
-- Parser --
------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

parseLp :: Parser (Lp Text)
parseLp = makeExprParser term table <?> "Propositional logic expression"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser (Lp Text)
term = parens parseLp <|> fromString `fmap` (L.lexeme sc (some alphaNumChar))

table :: [[Operator Parser (Lp Text)]]
table = [ [ prefix "~" Bar ]
        , [ binary "/\\" Conj
          , binary "\\/" Disj
          , binary "=>" Impl
          ]
        ]

binary :: Text -> (Lp Text -> Lp Text -> Lp Text) -> Operator Parser (Lp Text)
binary name f = InfixR (f <$ symbol name)

prefix :: Text -> (Lp Text -> Lp Text) -> Operator Parser (Lp Text)
prefix name f = Prefix (f <$ symbol name)

------------
-- Prover --
------------

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

proveLp :: Eq a => Lp a -> Bool
proveLp = closeLp mempty . growLp . pure . Bar
