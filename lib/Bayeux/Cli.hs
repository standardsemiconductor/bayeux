module Bayeux.Cli
  ( Cli(..)
  , Demo(..)
  , Prove(..)
  , Input(..)
  ) where

data Cli = CliDemo Demo Bool
         | CliProve Prove
         | CliCom
  deriving (Eq, Read, Show)

data Demo = FiatLux
          | RgbCounter
          | RgbCycle
          | Hello
  deriving (Eq, Read, Show)

data Prove = Prove
  { input    :: Input
  , tableaux :: Bool
  }
  deriving (Eq, Read, Show)

data Input = FileInput FilePath
           | StdInput
  deriving (Eq, Read, Show)
