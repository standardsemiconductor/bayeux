module Bayeux.Cli
  ( Cmd(..)
  , Cli(..)
  , Demo(..)
  , Prove(..)
  , Input(..)
  ) where

data Cmd = Prog
         | Synth
         | Clean
  deriving (Eq, Read, Show)

data Cli = CliDemo Cmd Demo
         | CliProve Prove
         | CliCom
  deriving (Eq, Read, Show)

data Demo = FiatLux
          | RgbCounter
          | RgbCycle
          | Hello
          | Echo
          | LedCtrl
          | EchoLine
  deriving (Eq, Read, Show)

data Prove = Prove
  { input    :: Input
  , tableaux :: Bool
  }
  deriving (Eq, Read, Show)

data Input = FileInput FilePath
           | StdInput
  deriving (Eq, Read, Show)
