{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Spi
  ( sbSpi
  , MonadSpi(..)
  , isW
  , sliceAddress
  , sliceData
  ) where

import Bayeux.Encode
import Bayeux.Rtl
import Bayeux.Signal
import Bayeux.Width
import Data.Text (Text)
import Data.Word

sbSpi
  :: CellId
  -> Text    -- ^ busAddr
  -> SigSpec -- ^ sbclki
  -> SigSpec -- ^ sbrwi
  -> SigSpec -- ^ sbstbi
  -> SigSpec -- ^ sbadri7
  -> SigSpec -- ^ sbadri6
  -> SigSpec -- ^ sbadri5
  -> SigSpec -- ^ sbadri4
  -> SigSpec -- ^ sbadri3
  -> SigSpec -- ^ sbadri2
  -> SigSpec -- ^ sbadri1
  -> SigSpec -- ^ sbadri0
  -> SigSpec -- ^ sbdati7
  -> SigSpec -- ^ sbdati6
  -> SigSpec -- ^ sbdati5
  -> SigSpec -- ^ sbdati4
  -> SigSpec -- ^ sbadti3
  -> SigSpec -- ^ sbdati2
  -> SigSpec -- ^ sbdati1
  -> SigSpec -- ^ sbdati0
  -> SigSpec -- ^ bi
  -> SigSpec -- ^ wi
  -> SigSpec -- ^ wcki
  -> SigSpec -- ^ wcsni
  -> SigSpec -- ^ sbdato7
  -> SigSpec -- ^ sbdato6
  -> SigSpec -- ^ sbdato5
  -> SigSpec -- ^ sbdato4
  -> SigSpec -- ^ sbdato3
  -> SigSpec -- ^ sbdato2
  -> SigSpec -- ^ sbdato1
  -> SigSpec -- ^ sbdato0
  -> SigSpec -- ^ sbacko
  -> SigSpec -- ^ spiirq
  -> SigSpec -- ^ spiwkup
  -> SigSpec -- ^ wo
  -> SigSpec -- ^ woe
  -> SigSpec -- ^ bo
  -> SigSpec -- ^ boe
  -> SigSpec -- ^ wcko
  -> SigSpec -- ^ wckoe
  -> SigSpec -- ^ bcsno3
  -> SigSpec -- ^ bcsno2
  -> SigSpec -- ^ bcsno1
  -> SigSpec -- ^ bcsno0
  -> SigSpec -- ^ bcsnoe3
  -> SigSpec -- ^ bcsnoe2
  -> SigSpec -- ^ bcsnoe1
  -> SigSpec -- ^ bcsnoe0
  -> Cell
sbSpi name a c rw stb a7 a6 a5 a4 a3 a2 a1 a0 di7 di6 di5 di4 di3 di2 di1 di0 bi wi wcki wcsni do7 do6 do5 do4 do3 do2 do1 do0 acko irq wkup wo woe bo boe wcko wckoe bcsno3 bcsno2 bcsno1 bcsno0 bcsnoe3 bcsnoe2 bcsnoe1 bcsnoe0 = Cell
  [AttrStmt "\\module_not_derived" $ ConstantInteger 1]
  (CellStmt "\\SB_SPI" name)
  [ CellParameter Nothing "\\BUS_ADDR74" $ ConstantString a
  , CellConnect "\\SBCLKI" c
  , CellConnect "\\SBRWI"  rw
  , CellConnect "\\SBSTBI" stb
  , CellConnect "\\SBADRI7" a7
  , CellConnect "\\SBADRI6" a6
  , CellConnect "\\SBADRI5" a5
  , CellConnect "\\SBADRI4" a4
  , CellConnect "\\SBADRI3" a3
  , CellConnect "\\SBADRI2" a2
  , CellConnect "\\SBADRI1" a1
  , CellConnect "\\SBADRI0" a0
  , CellConnect "\\SBDATI7" di7
  , CellConnect "\\SBDATI6" di6
  , CellConnect "\\SBDATI5" di5
  , CellConnect "\\SBDATI4" di4
  , CellConnect "\\SBDATI3" di3
  , CellConnect "\\SBDATI2" di2
  , CellConnect "\\SBDATI1" di1
  , CellConnect "\\SBDATI0" di0
  , CellConnect "\\MI"      bi
  , CellConnect "\\SI"      wi
  , CellConnect "\\SCKI"    wcki
  , CellConnect "\\SCSNI"   wcsni
  , CellConnect "\\SBDATO7" do7
  , CellConnect "\\SBDATO6" do6
  , CellConnect "\\SBDATO5" do5
  , CellConnect "\\SBDATO4" do4
  , CellConnect "\\SBDATO3" do3
  , CellConnect "\\SBDATO2" do2
  , CellConnect "\\SBDATO1" do1
  , CellConnect "\\SBDATO0" do0
  , CellConnect "\\SBACKO"  acko
  , CellConnect "\\SPIIRQ"  irq
  , CellConnect "\\SPIWKUP" wkup
  , CellConnect "\\SO"      wo
  , CellConnect "\\SOE"     woe
  , CellConnect "\\MO"      bo
  , CellConnect "\\MOE"     boe
  , CellConnect "\\SCKO"    wcko
  , CellConnect "\\SCKOE"   wckoe
  , CellConnect "\\MCSNO3"  bcsno3
  , CellConnect "\\MCSNO2"  bcsno2
  , CellConnect "\\MCSNO1"  bcsno1
  , CellConnect "\\MCSNO0"  bcsno0
  , CellConnect "\\MCSNOE3" bcsnoe3
  , CellConnect "\\MCSNOE2" bcsnoe2
  , CellConnect "\\MCSNOE1" bcsnoe1
  , CellConnect "\\MCSNOE0" bcsnoe0
  ]
  CellEndStmt

data Addr = Cr0
          | Cr1
          | Cr2
          | Br
          | Txdr
          | Rxdr
          | Csr
          | Sr
          | Irq
          | Irqen
  deriving (Eq, Read, Show)

instance Encode Addr where
  encode = \case
    Cr0   -> [B1, B0, B0, B0]
    Cr1   -> [B1, B0, B0, B1]
    Cr2   -> [B1, B0, B1, B0]
    Br    -> [B1, B0, B1, B1]
    Txdr  -> [B1, B1, B0, B1]
    Rxdr  -> [B1, B1, B1, B0]
    Csr   -> [B1, B1, B1, B1]
    Sr    -> [B1, B1, B0, B0]
    Irq   -> [B0, B1, B1, B0]
    Irqen -> [B0, B1, B1, B1]

instance Width Addr where
  width _ = 4

-- | Request
data Req = R Addr
         | W Addr
             Word8 -- ^ data

instance Encode Req where
  encode = \case
    R a   -> [B0] <> encode a <> replicate 8 B0
    W a d -> [B1] <> encode a <> encode d

instance Width Req where
  width _ = 13

isW :: Sig Req -> Sig Bool
isW = slice 12 12

sliceAddress :: Sig Req -> Sig Word8
sliceAddress = slice 11 8

sliceData :: Sig Req -> Sig Word8
sliceData = slice 7 0

class MonadSpi m where
  spi :: Text -> Sig (Maybe Req) -> m (Sig (Maybe Word8))

instance MonadSpi Rtl where
  spi busAddr req = do
    name <- fromString . ("\\SB_SPI_INST" <>) . show <$> fresh
    let c   = SigSpecWireId "\\clk"
        rw  = isW req
        stb = isValid req
        a7  = slice 7 7 $ sliceAddress req
        a6  = slice 6 6 $ sliceAddress req
        a5  = slice 5 5 $ sliceAddress req
        a4  = slice 4 4 $ sliceAddress req
        a3  = slice 3 3 $ sliceAddress req
        a2  = slice 2 2 $ sliceAddress req
        a1  = slice 1 1 $ sliceAddress req
        a0  = slice 0 0 $ sliceAddress req
        di7 = slice 7 7 $ sliceData    req
        di6 = slice 6 6 $ sliceData    req
        di5 = slice 5 5 $ sliceData    req
        di4 = slice 4 4 $ sliceData    req
        di3 = slice 3 3 $ sliceData    req
        di2 = slice 2 2 $ sliceData    req
        di1 = slice 1 1 $ sliceData    req
        di0 = slice 0 0 $ sliceData    req
    tell
      [ ModuleBodyCell $
          sbSpi name busAddr c rw stb a7 a6 a5 a4 a3 a2 a1 a0 di7 di6 di5 di4 di3 di2 di1 di0 bi wi wcki wcsni do7 do6 do5 do4 do3 do2 do1 do0 acko irq wkup wo woe bo boe wcko wckoe bcsno3 bcsno2 bcsno1 bcsno0 bcsnoe3 bcsnoe2 bcsnoe1 bcsnoe0
      ]
    return $ Sig $ acko <> do7 <> do6 <> do5 <> do4 <> do3 <> do2 <> do1 <> do0
