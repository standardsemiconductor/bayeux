{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Spi
  ( sbSpi
  , MonadSpi(..)
  , isW
  , sliceAddr
  , sliceData
  ) where

import Bayeux.Encode
import Bayeux.Ice40.IO
import Bayeux.Rtl
import Bayeux.Signal
import Bayeux.Width
import Control.Monad.Writer
import Data.Array
import Data.Finite
import Data.String
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

sliceAddr :: Sig Req -> Sig (Array (Finite 4) Bool)
sliceAddr = slice 11 8

sliceData :: Sig Req -> Sig Word8
sliceData = slice 7 0

class MonadSpi m where
  spi :: Text -> Sig (Maybe Req) -> m (Sig (Maybe Word8))

instance MonadSpi Rtl where
  spi busAddr req = do
    name <- fromString . ("\\SB_SPI_INST" <>) . show <$> fresh
    let c   = SigSpecWireId "\\clk"
        rw  = spec $ isW $ sliceValue req
        stb = spec $ sliceValid req
        reqAddr n = spec $ slice n n $ sliceAddr $ sliceValue req
        reqData n = spec $ slice n n $ sliceData $ sliceValue req
        a7  = SigSpecConstant $ ConstantValue $ Value 1 [B0] -- TODO: busAddr!
        a6  = SigSpecConstant $ ConstantValue $ Value 1 [B0]
        a5  = SigSpecConstant $ ConstantValue $ Value 1 [B0]
        a4  = SigSpecConstant $ ConstantValue $ Value 1 [B0]
        a3  = reqAddr 3
        a2  = reqAddr 2
        a1  = reqAddr 1
        a0  = reqAddr 0
        di7 = reqData 7
        di6 = reqData 6
        di5 = reqData 5
        di4 = reqData 4
        di3 = reqData 3
        di2 = reqData 2
        di1 = reqData 1
        di0 = reqData 0
    bi      <- freshWire 1
    wi      <- freshWire 1
    wcki    <- freshWire 1
    wcsni   <- freshWire 1
    do7     <- freshWire 1
    do6     <- freshWire 1
    do5     <- freshWire 1
    do4     <- freshWire 1
    do3     <- freshWire 1
    do2     <- freshWire 1
    do1     <- freshWire 1
    do0     <- freshWire 1
    acko    <- freshWire 1
    irq     <- freshWire 1
    wkup    <- freshWire 1
    wo      <- freshWire 1
    woe     <- freshWire 1
    bo      <- freshWire 1
    boe     <- freshWire 1
    wcko    <- freshWire 1
    wckoe   <- freshWire 1
    bcsno3  <- freshWire 1
    bcsno2  <- freshWire 1
    bcsno1  <- freshWire 1
    bcsno0  <- freshWire 1
    bcsnoe3 <- freshWire 1
    bcsnoe2 <- freshWire 1
    bcsnoe1 <- freshWire 1
    bcsnoe0 <- freshWire 1
    biwo    <- inout "\\spi_biwo"
    bowi    <- inout "\\spi_bowi"
    wck     <- inout "\\spi_wck"
    cs      <- inout "\\spi_cs"
    tell
      [ ModuleBodyCell $
          sbSpi name busAddr c rw stb a7 a6 a5 a4 a3 a2 a1 a0 di7 di6 di5 di4 di3 di2 di1 di0 bi wi wcki wcsni do7 do6 do5 do4 do3 do2 do1 do0 acko irq wkup wo woe bo boe wcko wckoe bcsno3 bcsno2 bcsno1 bcsno0 bcsnoe3 bcsnoe2 bcsnoe1 bcsnoe0
      , ModuleBodyCell $ biwoIO biwo woe    wo    bi
      , ModuleBodyCell $ bowiIO bowi boe    bo    wi
      , ModuleBodyCell $ wckIO  wck  wckoe  wcko  wcki
      , ModuleBodyCell $ csIO   cs   bcsnoe0 bcsno0 wcsni
      ]
    return $ Sig $ acko <> do7 <> do6 <> do5 <> do4 <> do3 <> do2 <> do1 <> do0

inout :: WireId -> Rtl SigSpec
inout wireId = do
  i <- fresh
  tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInout i] wireId]
  return $ SigSpecWireId wireId

biwoIO
  :: SigSpec -- ^ package pin
  -> SigSpec -- ^ output enable
  -> SigSpec -- ^ D_OUT_0
  -> SigSpec -- ^ D_IN_0
  -> Cell
biwoIO pp woe wo bi = sbIO
  "\\SB_IO_BIWO"
  (ConstantValue $ Value 6 [B0, B0, B0, B0, B0, B1]) -- pin type
  (ConstantValue $ Value 1 [B0]) -- pullup
  (ConstantValue $ Value 1 [B0]) -- neg trigger
  (ConstantString "SB_LVCMOS")
  pp
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecWireId "\\clk")
  (SigSpecWireId "\\clk")
  woe
  wo
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  bi
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])

bowiIO
  :: SigSpec -- ^ package pin
  -> SigSpec -- ^ output enable
  -> SigSpec -- ^ D_OUT_0
  -> SigSpec -- ^ D_IN_0
  -> Cell
bowiIO pp boe bo wi = sbIO
  "\\SB_IO_BOWI"
  (ConstantValue $ Value 6 [B1, B0, B1, B0, B0, B1]) -- pin type
  (ConstantValue $ Value 1 [B0]) -- pullup
  (ConstantValue $ Value 1 [B0]) -- neg trigger
  (ConstantString "SB_LVCMOS")
  pp
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecWireId "\\clk")
  (SigSpecWireId "\\clk")
  boe
  bo
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  wi
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])

wckIO
  :: SigSpec
  -> SigSpec
  -> SigSpec
  -> SigSpec
  -> Cell
wckIO pp wckoe wcko wcki = sbIO
  "\\SB_IO_WCK"
  (ConstantValue $ Value 6 [B1, B0, B1, B0, B0, B1]) -- pin type
  (ConstantValue $ Value 1 [B1]) -- pullup
  (ConstantValue $ Value 1 [B0]) -- neg trigger
  (ConstantString "SB_LVCMOS")
  pp
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecWireId "\\clk")
  (SigSpecWireId "\\clk")
  wckoe
  wcko
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  wcki
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])

csIO
  :: SigSpec
  -> SigSpec
  -> SigSpec
  -> SigSpec
  -> Cell
csIO pp bcsnoe bcsno wcsni = sbIO
  "\\SB_IO_CS"
  (ConstantValue $ Value 6 [B1, B0, B1, B0, B0, B1]) -- pin type
  (ConstantValue $ Value 1 [B1]) -- pullup
  (ConstantValue $ Value 1 [B0]) -- neg trigger
  (ConstantString "SB_LVCMOS")
  pp
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  (SigSpecWireId "\\clk")
  (SigSpecWireId "\\clk")
  bcsnoe
  bcsno
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
  wcsni
  (SigSpecConstant $ ConstantValue $ Value 1 [B0])
