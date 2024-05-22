{-# LANGUAGE DeriveTraversable #-}

module Bayeux.Tableaux
  ( Tableaux(..)
  ) where

data Tableaux a = Leaf   a
                | Stem   a (Tableaux a)
                | Branch a (Tableaux a) (Tableaux a)
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)
