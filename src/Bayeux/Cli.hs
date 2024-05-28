module Bayeux.Cli
  ( Cli(..)
  , Input(..)
  ) where

data Cli = Cli
  { input    :: Input
  , tableaux :: Bool
  }
  deriving (Eq, Read, Show)

data Input = FileInput FilePath
           | StdInput
  deriving (Eq, Read, Show)
