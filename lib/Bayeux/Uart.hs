{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  , bufEcho
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, unary)
import Bayeux.Signal
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

instance MonadUart Rtl where
  transmit baud byte = void $ process $ \txFsm -> do
    isStart <- txFsm === sig False
    txCtr <- process $ \txCtr -> do
      ctrDone <- txCtr === sig baud
      txCtr' <- C.inc txCtr
      ifm [ isStart `thenm` sig 0
          , ctrDone `thenm` sig 0
          , elsem txCtr'
          ]
    ctrDone <- txCtr === sig baud
    notDone <- C.logicNot ctrDone
    txIx <- process $ \txIx -> do
      isEmpty <- txIx === sig 9
      txIx'   <- C.inc txIx
      ifm [ notDone `thenm` txIx
          , isEmpty `thenm` sig (0 :: Word8)
          , elsem txIx'
          ]
    isStartFrame <- txIx === sig 0
    isEndFrame   <- txIx === sig 9
    buf <- process $ \buf -> do
      buf' <- buf `C.shr` sig True
      ifm [ isStart      `thenm` sliceValue byte
          , isStartFrame `thenm` buf
          , notDone      `thenm` buf
          , elsem buf'
          ]
    e <- buf `at` 0
    output "\\tx" =<< ifm
      [ isStart      `thenm` sig True
      , isStartFrame `thenm` sig False
      , isEndFrame   `thenm` sig True
      , elsem e
      ]
    txFsm' <- C.logicNot =<< ctrDone `C.logicAnd` isEndFrame
    mux isStart txFsm' $ sliceValid byte

  receive baud rx = do
    rxLow  <- rx === sig False
    rxHigh <- C.logicNot rxLow
    fmap snd $ machine $ \s -> do
      isIdle  <- rxFsm s === idle
      isStart <- rxFsm s === start
      isRecv  <- rxFsm s === recv
      isBaudHalf        <- rxCtr s === sig (baud `shiftR` 1)
      isBaudHalfRxStart <- isBaudHalf `C.logicAnd` isStart
      isBaud            <- rxCtr s === sig baud
      isBaudRxRecv      <- isBaud `C.logicAnd` isRecv
      buf <- buffer $ toMaybeSig isBaudRxRecv rx
      gotoRxStart <- rxLow `C.logicAnd` isIdle
      gotoRxRecv  <- rxLow `C.logicAnd` isBaudHalfRxStart
      gotoRxStop  <- sliceValid buf `logicAnd` isRecv
      gotoRxIdle  <- (rxHigh `logicAnd` isBaudHalfRxStart)
                       .|| (pure isBaud .&& rxFsm s === stop)
      rxFsm' <- ifm
        [ gotoRxStart `thenm` start
        , gotoRxRecv  `thenm` recv
        , gotoRxStop  `thenm` stop
        , gotoRxIdle  `thenm` idle
        , elsem $ rxFsm s
        ]
      rxCtr1 <- C.inc $ rxCtr s
      rxCtr' <- ifm
        [ isIdle            `thenm` sig 0
        , isBaudHalfRxStart `thenm` sig 0
        , isBaud            `thenm` sig 0
        , elsem rxCtr1
        ]
      return (Sig{ spec = pad <> spec rxCtr' <> spec rxFsm' }, packMaybe buf)
    where
      pad = "8'00000000"
      idle  = sig 0
      start = sig 1
      recv  = sig 2
      stop  = sig 3
      rxFsm :: Sig Word32 -> Sig Word8
      rxFsm = slice 7 0
      rxCtr :: Sig Word32 -> Sig Word16
      rxCtr = slice 23 8
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
