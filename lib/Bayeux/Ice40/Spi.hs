{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Ice40.Spi
  ( sbSpi
  ) where

import Bayeux.Rtl
import Data.Text (Text)

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