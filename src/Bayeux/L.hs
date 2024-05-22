{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

-- | First-Order Logic
module Bayeux.L
  ( L(..)
  ) where

import Bayeux.Tableaux
import Data.Functor
import Data.List.NonEmpty

data L a = Fun a (NonEmpty a)
         | Bar (L a)
         | Conj (L a) (L a)
         | Disj (L a) (L a)
         | Impl (L a) (L a)
         | All a (L a)
         | Exist a (L a)
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

infixr 3 /\
(/\) :: L a -> L a -> L a
(/\) = Conj

infixr 2 \/
(\/) :: L a -> L a -> L a
(\/) = Disj

infixr 1 ==>
(==>) :: L a -> L a -> L a
(==>) = Impl

------------
-- Prover --
------------

data Node a = Parameter Integer
            | Variable  a
  deriving (Eq, Read, Show)

grow :: Eq a => Integer -> NonEmpty (L (Node a)) -> Tableaux (L (Node a))
grow p (e :| es) =  case e of
  Fun{}          -> case nonEmpty es of
    Nothing      -> Leaf e
    Just es'     -> Stem e $ grow p es'
  Bar Fun{}      -> case nonEmpty es of
    Nothing      -> Leaf e
    Just es'     -> Stem e $ grow p es'
  Bar (Bar x)    -> Stem e $ grow p  $ x :| es
  Conj x y       -> Stem e $ grow p  $ x <| y :| es
  Bar (Disj x y) -> Stem e $ grow p  $ Bar x <| Bar y :| es
  Bar (Impl x y) -> Stem e $ grow p  $ x <| Bar y :| es
  All (Variable x) a         -> Stem e $ grow p  $ sub x p a :| es
  Bar (Exist (Variable x) a) -> Stem e $ grow p  $ Bar (sub x p a) :| es
  Exist (Variable x) a       -> Stem e $ grow p' $ sub x p' a :| es
  Bar (All (Variable x) a)   -> Stem e $ grow p' $ Bar (sub x p' a) :| es
  Bar (Conj x y)  ->
    let l = Bar x :| es
        r = Bar y :| es
    in Branch e (grow p l) (grow p r)
  Disj x y ->
    let l = x :| es
        r = y :| es
    in Branch e (grow p l) (grow p r)
  Impl x y ->
    let l = Bar x :| es
        r =     y :| es
    in Branch e (grow p l) (grow p r)
  where
    p' = p + 1

sub :: Eq a => a -> Integer -> L (Node a) -> L (Node a)
sub x p = \case
  Fun a vs -> Fun a $ subVar <$> vs
  Bar  a   -> Bar $ sub x p a
  Conj a b -> Conj (sub x p a) (sub x p b)
  Disj a b -> Disj (sub x p a) (sub x p b)
  Impl a b -> Impl (sub x p a) (sub x p b)
  All   (Variable y) a | x == y    -> All   (Variable y) a
                       | otherwise -> All   (Variable y) $ sub x p a
  Exist (Variable y) a | x == y    -> Exist (Variable y) a
                       | otherwise -> Exist (Variable y) $ sub x p a
  where
    subVar = \case
      Variable v | v == x -> Parameter p
      other               -> other

-- pg. 44
testSubA :: L (Node String)
testSubA = sub "x" 0 $ fmap Variable $ All "x" (Fun "P" ["x"]) \/ Bar (Exist "y" (Fun "Q" ["x", "y"]))

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
prove = close mempty . grow 0 . pure . fmap Variable . Bar

testProof = prove $ All "x" (Fun "P" ["x"] ==> Fun "Q" ["x"]) ==> All "x" (Fun "P" ["x"]) ==> All "x" (Fun "Q" ["x"])
