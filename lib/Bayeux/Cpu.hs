{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module Bayeux.Cpu
  ( Instr(..)
  , Prog(..)
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import Bayeux.Encode
import Bayeux.Signal
import Bayeux.Uart
import Bayeux.Width
import Data.Array
import Data.Char
import Data.Finite
import Data.Word

newtype Instr = PutChr Word8
  deriving (Eq, Read, Show)

instance Width Instr where
  width _ = 9

instance Encode Instr where
  encode (PutChr b) = B0 : encode b

sliceOpcode :: Sig Instr -> Sig (Finite 1)
sliceOpcode = slice 8 8

newtype Prog = Prog [Instr]
  deriving (Eq, Read, Show)

cpu :: Sig (Maybe Instr) -> m (Sig (Maybe Word8))
cpu instrM = ifm
  [ (pure . sliceValid) instrM `thenm` (fmap justSig . execute) (sliceValue instrM)
  , elsem $ val Nothing
  ]
  where
    execute :: Sig Instr -> m (Sig Word8)
    execute instr = patm (sliceOpcode instr)
      [ 0 ~> undefined
      , wildm undefined
      ]

justSig :: Sig a -> Sig (Maybe a)
justSig s = Sig $ (spec . sig) True <> spec s

soc :: m ()
soc = transmit 624 =<< cpu =<< (cobuffer . sig) prog
  where
    prog = embed $ Prog $ PutChr . fromIntegral . ord <$> "Hello World!"

embed :: Prog -> Array (Finite 12) Instr
embed (Prog instrs) = listArray (0, 11) instrs
