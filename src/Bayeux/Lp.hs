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
  ) where

import Control.Monad.Combinators.Expr
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Lp a = Bv a
          | Bar  (Lp a)
          | Conj (Lp a) (Lp a)
          | Disj (Lp a) (Lp a)
          | Impl (Lp a) (Lp a)
  deriving (Eq, Read, Show)

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

prettyLp :: Show a => Lp a -> String
prettyLp = \case
  Bv b     -> show b
  Bar x    -> "~" <> prettySub x
  Conj x y -> prettySub x <> " " <> "/\\" <> " " <> prettySub y
  Disj x y -> prettySub x <> " " <> "\\/" <> " " <> prettySub y
  Impl x y -> prettySub x <> " => " <> prettySub y
  where
    prettySub = \case
      e@Bv{} -> prettyLp e
      e      -> "(" <> prettyLp e <> ")"

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
term = parens parseLp <|> fromString `fmap` some alphaNumChar

table :: [[Operator Parser (Lp Text)]]
table = [ [ prefix "~" Bar ]
        , [ binary "/\\" Conj
          , binary "\\/" Disj
          , binary "==>" Impl
          ]
        ]

binary :: Text -> (Lp Text -> Lp Text -> Lp Text) -> Operator Parser (Lp Text)
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Lp Text -> Lp Text) -> Operator Parser (Lp Text)
prefix name f = Prefix (f <$ symbol name)
