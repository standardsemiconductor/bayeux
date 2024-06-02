{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

-- | First-Order Logic
module Bayeux.L
  ( L(..)
--  , (/\)
--  , (\/)
--  , (==>)
--  , Node(..)
--  , render
--  , unfold
--  , prove
  ) where

import Bayeux.Tableaux
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Data.Functor.Identity
import Data.Monoid (Endo(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Text

data L a = Var Text
         | Fun a a
         | Bar a
         | Conj a a
         | Disj a a
         | Impl a a
         | All   a a
         | Exist a a
  deriving (Eq, Foldable, Functor, Generic1, Read, Show, Traversable, Unifiable)

type Term = UTerm L Text
type Failure = UFailure L IntVar
type BindingState = IntBindingState L
type LMonad = ExceptT Failure (IntBindingT L Identity)
--var :: Text -> Term
var = UVar . Var
fun n a = UTerm $ Fun n a
bar = UTerm . Bar
conj x y = UTerm $ Conj x y

--getFreeVar :: BindingMonad t v m => m v
--getFreeVar = lift $ UVar <$> freeVar
runUnifier :: LMonad a -> (Either Failure a, BindingState)
runUnifier = runIdentity . runIntBindingT . runExceptT

ex1 = runUnifier $ do
  let pa = UTerm $ Fun (UVar $ Var "P") (UVar $ Var "a")
      pc = UTerm $ Fun (UVar $ Var "P") (UVar $ Var "c")
  pa =:= pc
{-
instance Eq a => Ord (L a) where
  Bar Bar{}   <= _ = True
  Conj{}      <= _ = True
  Bar Disj{}  <= _ = True
  Bar Impl{}  <= _ = True
  Fun{}       <= _ = True
  Bar Fun{}   <= _ = True
  Bar All{}   <= _ = True
  Exist{}     <= _ = True
  Bar Exist{} <= _ = False
  All{}       <= _ = False
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
  => Integer -- ^ unique parameter
  -> Set (L (Node a)) -- ^ gamma
  -> Set (L (Node a))
  -> Tableaux (L (Node a))
unfold p g s
  | S.null s = unfold p g $ S.fromList [ g' | i <- [0..p] 
                                            , l <- S.toList g
                                            ]
  | otherwise = case e of
  -- alpha
  Fun{}     | S.null s' -> Leaf e
            | otherwise -> Stem e $ unfold p g s'
  Bar Fun{} | S.null s' -> Leaf e
            | otherwise -> Stem e $ unfold p g s'
  Bar (Bar x)    -> Stem e $ unfold p g $ S.insert x s'
  Conj x y       -> Stem e $ unfold p g $ S.insert x $ S.insert y s'
  Bar (Disj x y) -> Stem e $ unfold p g $ S.insert (Bar x) $ S.insert (Bar y) s'
  Bar (Impl x y) -> Stem e $ unfold p g $ S.insert x $ S.insert (Bar y) s'
  -- gamma
  All (Var x) a         -> Stem e $ unfold p (S.insert e g) s'
  Bar (Exist (Var x) a) -> Stem e $ unfold p (S.insert e g) s'
  -- delta
  Exist (Var x) a     -> Stem e $ unfold p' g $ S.insert (sub x p' a) s'
  Bar (All (Var x) a) -> Stem e $ unfold p' g $ S.insert (Bar $ sub x p' a) s'
  -- beta
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
-}
