{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Language of propositions
module Bayeux.Lp
  ( Lp(..)
  , (/\), (\/), (==>)
  , isSignedBv
  , eval
  , render
  , parse
  , unfold
  , close
  , prove
  ) where

import Bayeux.Tableaux
import Control.Monad.Combinators.Expr
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import Data.Void
import Prettyprinter hiding (parens)
import Prettyprinter.Render.Text
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Lp a = Bv a
          | Bar  (Lp a)
          | Conj (Lp a) (Lp a)
          | Disj (Lp a) (Lp a)
          | Impl (Lp a) (Lp a)
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

instance Eq a => Ord (Lp a) where
  Bar Bar{}  <= _ = True
  Conj{}     <= _ = True
  Bar Disj{} <= _ = True
  Bar Impl{} <= _ = True
  Bv{}       <= _ = True
  Bar Bv{}   <= _ = True
  _          <= _ = False

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

eval :: Lp Bool -> Bool
eval = \case
  Bv b     -> b
  Bar  x   -> not $ eval x
  Conj x y -> eval x && eval y
  Disj x y -> eval x || eval y
  Impl x y -> not (eval x) || eval y

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

render :: Pretty a => Lp a -> Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

------------
-- Parser --
------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = L.symbol sc

parse :: Parser (Lp Text)
parse = makeExprParser term table <?> "Propositional logic expression"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser (Lp Text)
term = parens parse <|> fromString `fmap` L.lexeme sc (some alphaNumChar)

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

unfold
  :: Eq a
  => Set (Lp a) -- ^ Assumptions
  -> Tableaux (Lp a)
unfold s = case e of
  Bv{}     | S.null s' -> Leaf e
           | otherwise -> Stem e $ unfold s'
  Bar Bv{} | S.null s' -> Leaf e
           | otherwise -> Stem e $ unfold s'
  Bar (Bar x)    -> Stem e $ unfold $ S.insert x s'
  Conj x y       -> Stem e $ unfold $ S.insert x $ S.insert y s'
  Bar (Disj x y) -> Stem e $ unfold $ S.insert (Bar x) $ S.insert (Bar y) s'
  Bar (Impl x y) -> Stem e $ unfold $ S.insert x $ S.insert (Bar y) s'
  Bar (Conj x y) ->
    let l = S.insert (Bar x) s'
        r = S.insert (Bar y) s'
    in Branch e (unfold l) (unfold r)
  Disj x y ->
    let l = S.insert x s'
        r = S.insert y s'
    in Branch e (unfold l) (unfold r)
  Impl x y ->
    let l = S.insert (Bar x) s'
        r = S.insert y s'
    in Branch e (unfold l) (unfold r)
  where
    (e, s') = S.deleteFindMin s

close :: Eq a => [Lp a] -> Tableaux (Lp a) -> Bool
close signedBvs = \case
  Leaf a -> hasContra $ a : signedBvs
  Stem a t
    | isSignedBv a -> close (a : signedBvs) t
    | otherwise    -> close signedBvs t
  Branch a l r
    | isSignedBv a -> let signedBvs' = a : signedBvs
                      in close signedBvs' l &&
                         close signedBvs' r
    | otherwise    -> close signedBvs l &&
                      close signedBvs r
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
prove = close mempty . unfold . S.singleton . Bar
