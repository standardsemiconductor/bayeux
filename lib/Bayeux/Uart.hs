{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  , bufEcho
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad
import Data.Array
import Data.Bits hiding (shift)
import Data.Finite
import Data.Word

class MonadUart m where
  transmit :: Word16 -- ^ baud
           -> Sig (Maybe Word8)
           -> m ()
  receive :: Word16   -- ^ baud
          -> Sig Bool -- ^ rx
          -> m (Sig (Maybe Word8))

data Fsm = Idle | Start | Recv | Stop
  deriving (Eq, Read, Show)

instance Width Fsm where
  width _ = 2

instance Encode Fsm where
  encode = \case
    Idle  -> [B0, B0]
    Start -> [B0, B1]
    Recv  -> [B1, B0]
    Stop  -> [B1, B1]

instance MonadUart Rtl where
  transmit baud byte = void $ process $ \txFsm -> do
    isStart <- txFsm === sig False
    txCtr <- process $ \txCtr -> ifm
      [ (pure isStart .|| txCtr === sig baud) `thenm` val 0
      , elsem $ inc txCtr
      ]
    ctrDone <- txCtr === sig baud
    notDone <- logicNot ctrDone
    txIx <- process $ \(txIx :: Sig Word8) -> ifm
      [ pure notDone     `thenm` pure txIx
      , (txIx === sig 9) `thenm` val 0
      , elsem $ inc txIx
      ]
    isStartFrame <- txIx === sig 0
    isEndFrame   <- txIx === sig 9
    buf <- process $ \buf -> do
      buf' <- buf `C.shr` sig True
      ifs [ isStart      `thens` sliceValue byte
          , isStartFrame `thens` buf
          , notDone      `thens` buf
          , elses buf'
          ]
    e <- buf `at` 0
    output "\\tx" =<< ifs
      [ isStart      `thens` sig True
      , isStartFrame `thens` sig False
      , isEndFrame   `thens` sig True
      , elses e
      ]
    txFsm' <- logicNot =<< ctrDone `logicAnd` isEndFrame
    mux isStart txFsm' $ sliceValid byte

  receive baud rx = do
    rxLow  <- rx === sig False
    rxHigh <- logicNot rxLow
    fmap snd $ machine $ \(s :: Sig (Word16, Fsm)) -> do
      let rxCtr = sliceFst s
          rxFsm = sliceSnd s
      isIdle  <- rxFsm === sig Idle
      isStart <- rxFsm === sig Start
      isRecv  <- rxFsm === sig Recv
      isBaudHalf        <- rxCtr === sig (baud `shiftR` 1)
      isBaudHalfRxStart <- isBaudHalf `logicAnd` isStart
      isBaud            <- rxCtr === sig baud
      isBaudRxRecv      <- isBaud `logicAnd` isRecv
      buf <- buffer $ toMaybeSig isBaudRxRecv rx
      gotoRxStart <- rxLow `logicAnd` isIdle
      gotoRxRecv  <- rxLow `logicAnd` isBaudHalfRxStart
      gotoRxStop  <- sliceValid buf `logicAnd` isRecv
      gotoRxIdle  <- (rxHigh `logicAnd` isBaudHalfRxStart)
                       .|| (pure isBaud .&& rxFsm === sig Stop)
      rxFsm' <- ifs
        [ gotoRxStart `thens` sig Start
        , gotoRxRecv  `thens` sig Recv
        , gotoRxStop  `thens` sig Stop
        , gotoRxIdle  `thens` sig Idle
        , elses rxFsm
        ]
      rxCtr' <- ifm
        [ ((isIdle `logicOr` isBaudHalfRxStart) .|| pure isBaud) `thenm` val 0
        , elsem $ inc rxCtr
        ]
      return (Sig{ spec = spec rxCtr' <> spec rxFsm' }, packMaybe buf)
    where
      packMaybe :: Sig (Maybe (Array (Finite 8) Bool)) -> Sig (Maybe Word8)
      packMaybe = Sig . spec

hello :: Monad m => MonadUart m => MonadSignal m => m (Sig Word32)
hello = process $ \timer -> do
  is5Sec <- timer === sig 60000000
  transmit 624 $ toMaybeSig is5Sec $ sig 0x61
  flip (mux is5Sec) (sig 0) =<< C.inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m ()
echo = transmit 624 =<< receive 624 =<< input "\\rx"

bufEcho :: Monad m => MonadBuffer m => MonadSignal m => MonadUart m => m ()
bufEcho = do
  b <- buf =<< receive 624 =<< input "\\rx"
  transmit 624 =<< cobuf b
  where
    buf :: MonadBuffer m => Sig (Maybe Word8) -> m (Sig (Maybe (Array (Finite 1) Word8)))
    buf = buffer
    cobuf :: MonadBuffer m => Sig (Maybe (Array (Finite 1) Word8)) -> m (Sig (Maybe Word8))
    cobuf = cobuffer
