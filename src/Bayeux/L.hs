{-# LANGUAGE DeriveTraversable #-}

-- | First-Order Logic
module Bayeux.L
  ( L(..)
  ) where

import Data.List.NonEmpty

data L a = Bar (L a)
         | Conj (L a) (L a)
         | Disj (L a) (L a)
         | Impl (L a) (L a)
         | All a (L a)
         | Exist a (L a)
         | Var a
         | Param a
         | Fun (NonEmpty (L a))
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)
