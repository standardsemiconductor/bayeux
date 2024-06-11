module Bayeux.Cli
  ( Cli(..)
  , Demo(..)
  , Prove(..)
  , Input(..)
  ) where

data Cli = CliDemo Demo
         | CliProve Prove
  deriving (Eq, Read, Show)

data Demo = FiatLux
  deriving (Eq, Read, Show)

data Prove = Prove
  { input    :: Input
  , tableaux :: Bool
  }
  deriving (Eq, Read, Show)

data Input = FileInput FilePath
           | StdInput
  deriving (Eq, Read, Show)
