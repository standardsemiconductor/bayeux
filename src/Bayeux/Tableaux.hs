{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}

module Bayeux.Tableaux
  ( Tableaux(..)
  , renderTableaux
  ) where

data Tableaux a = Leaf   a
                | Stem   a (Tableaux a)
                | Branch a (Tableaux a) (Tableaux a)
  deriving (Eq, Foldable, Functor, Read, Show, Traversable)

renderTableaux :: Tableaux String -> String
renderTableaux = unlines . draw
  where
    draw = \case
      Leaf a       -> lines a
      Stem a t     -> lines a ++ drawSubTrees [t]
      Branch a l r -> lines a ++ drawSubTrees [l, r]
    drawSubTrees = \case
      []   -> []
      [t]  -> "\x2502" : shift "\x2502" "" (draw t)
      t:ts -> "\x2502" : shift "\x251C\x2500\x2510" "\x2502 " (draw t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
