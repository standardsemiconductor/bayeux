{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

-- | First-Order Logic
module Bayeux.L
  ( L(..)
  , (/\)
  , (\/)
  , (==>)
  , Node(..)
  , render
  , unfold
  , prove
  ) where

import Bayeux.Tableaux
import Data.Monoid (Endo(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text

data L a = Fun a [a]
         | Bar (L a)
         | Conj (L a) (L a)
         | Disj (L a) (L a)
         | Impl (L a) (L a)
         | All   a (L a)
         | Exist a (L a)
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

instance Eq a => Ord (L a) where
  Bar Bar{}   <= _ = True
  Conj{}      <= _ = True
  Bar Disj{}  <= _ = True
  Bar Impl{}  <= _ = True
  Bar All{}   <= _ = True
  Bar Exist{} <= _ = True
  All{}       <= _ = True
  Exist{}     <= _ = True
  Fun{}       <= _ = True
  Bar Fun{}   <= _ = True
  _           <= _ = False

infixr 3 /\
(/\) :: L a -> L a -> L a
(/\) = Conj

infixr 2 \/
(\/) :: L a -> L a -> L a
(\/) = Disj

infixr 1 ==>
(==>) :: L a -> L a -> L a
(==>) = Impl

instance Pretty a => Pretty (L a) where
  pretty = \case
    Fun   a ns -> pretty a <+> hsep (pretty <$> ns)
    Bar   x    -> "~" <> prettySub x
    Conj  x y  -> prettySub x <+> "/\\" <+> prettySub y
    Disj  x y  -> prettySub x <+> "\\/" <+> prettySub y
    Impl  x y  -> prettySub x <+> "=>"  <+> prettySub y
    All   a x  -> ".A" <+> pretty a <+> pretty x
    Exist a x  -> ".E" <+> pretty a <+> pretty x
    where
      prettySub = \case
        e@Fun{} -> pretty e
        e       -> "(" <> pretty e <> ")"

render :: Pretty a => L a -> Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

------------
-- Prover --
------------

data Node a = Parameter Integer
            | Var a
  deriving (Eq, Read, Show)

instance Pretty a => Pretty (Node a) where
  pretty = \case
    Parameter i -> pretty i
    Var a       -> pretty a

unfold
  :: Eq a
  => Integer
  -> Set (L (Node a)) -- ^ gamma
  -> Set (L (Node a))
  -> Tableaux (L (Node a))
unfold p g s = case e of
  Fun{}     | S.null s' -> Leaf e
            | otherwise -> Stem e $ unfold p g s'
  Bar Fun{} | S.null s' -> Leaf e
            | otherwise -> Stem e $ unfold p g s'
  Bar (Bar x)    -> Stem e $ unfold p g $ S.insert x s'
  Conj x y       -> Stem e $ unfold p g $ S.insert x $ S.insert y s'
  Bar (Disj x y) -> Stem e $ unfold p g $ S.insert (Bar x) $ S.insert (Bar y) s'
  Bar (Impl x y) -> Stem e $ unfold p g $ S.insert x $ S.insert (Bar y) s'
  All (Var x) a         -> Stem e $ unfold p (S.insert e g) $ S.insert (sub x p a) s'
  Bar (Exist (Var x) a) -> Stem e $ unfold p (S.insert e g) $ S.insert (Bar $ sub x p a) s'
  Exist (Var x) a     -> Stem e $ unfold p' g $ delta id  x a $ gamma p g s'
  Bar (All (Var x) a) -> Stem e $ unfold p' g $ delta Bar x a $ gamma p g s'
  Bar (Conj x y) ->
    let l = S.insert (Bar x) s'
        r = S.insert (Bar y) s'
    in Branch e (unfold p g l) (unfold p g r)
  Disj x y ->
    let l = S.insert x s'
        r = S.insert y s'
    in Branch e (unfold p g l) (unfold p g r)
  Impl x y ->
    let l = S.insert (Bar x) s'
        r = S.insert y s'
    in Branch e (unfold p g l) (unfold p g r)
  where
    (e, s') = S.deleteFindMin s
    p' = p + 1
    delta f x a = appEndo $ foldMap Endo [ S.insert (f (sub x i a)) | i <- [0..p] ]

gamma :: Eq a => Integer -> Set (L (Node a)) -> Set (L (Node a)) -> Set (L (Node a))
gamma i g = appEndo $ foldMap updates g
  where
    updates = \case
      Bar (Exist (Var x) a) -> Endo $ S.insert $ Bar $ sub x i a
      All (Var x) a         -> Endo $ S.insert $ sub x i a
      _                     -> mempty

sub :: Eq a => a -> Integer -> L (Node a) -> L (Node a)
sub x p = \case
  Fun a vs -> Fun a $ subVar <$> vs
  Bar  a   -> Bar $ sub x p a
  Conj a b -> Conj (sub x p a) (sub x p b)
  Disj a b -> Disj (sub x p a) (sub x p b)
  Impl a b -> Impl (sub x p a) (sub x p b)
  All   (Var y) a | x == y    -> All   (Var y) a
                  | otherwise -> All   (Var y) $ sub x p a
  Exist (Var y) a | x == y    -> Exist (Var y) a
                  | otherwise -> Exist (Var y) $ sub x p a
  where
    subVar = \case
      Var v | v == x -> Parameter p
      other          -> other

close :: Eq a => [L a] -> Tableaux (L a) -> Bool
close signedFuns = \case
  Leaf a -> hasContra $ a : signedFuns
  Stem a t
    | isSignedFun a -> close (a : signedFuns) t
    | otherwise     -> close signedFuns t
  Branch a l r
    | isSignedFun a -> let signedFuns' = a : signedFuns
                       in close signedFuns' l &&
                          close signedFuns' r
    | otherwise    -> close signedFuns l &&
                      close signedFuns r
  where
    hasContra :: Eq a => [L a] -> Bool
    hasContra funs = any (flip elem funs . invert) funs
      where
        invert :: L a -> L a
        invert = \case
          Bar v@Fun{} -> v
          v@Fun{}     -> Bar v
          lp          -> lp

isSignedFun :: L a -> Bool
isSignedFun = \case
  Bar Fun{} -> True
  Fun{}     -> True
  _         -> False

prove :: Eq a => L a -> Bool
prove = close mempty . unfold 0 mempty . S.singleton . fmap Var . Bar
