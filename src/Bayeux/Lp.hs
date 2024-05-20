{-# LANGUAGE LambdaCase #-}

-- | Language of propositions
module Bayeux.Lp
  ( Lp(..)
  , (/\), (\/), (==>)
  , isSignedBv
  , evalLp
  , prettyLp
  ) where

import Data.String

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

parseLp = makeExprParser term table <?> "Propositional logic expression"

term = parens parseLp <|> some alphaNum

table :: [[Operator m a]]
table = [ [ prefix "~" Bar ]
        , [ binary "/\\" Conj
          , binary "\\/" Disj
          , binary "==>" Impl
          ]
        ]

binary :: _ -> (_ -> Lp a) -> Operator m a
binary name f = InfixL (f <$ symbol name)

prefix :: _ -> (_ -> Lp a) -> Operator m a
prefix name f = Prefix (f <$ symbol name)
