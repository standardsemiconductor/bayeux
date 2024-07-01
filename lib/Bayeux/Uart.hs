{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  ) where

import Bayeux.Buffer (buffer)
import Bayeux.Cell ((===), ifm, elsem, thenm)
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
           -> OptSig Word8
           -> m ()
  receive :: Word16   -- ^ baud
          -> Sig Bool -- ^ rx
          -> m (OptSig Word8)

instance MonadUart Rtl where
  transmit baud byte = void $ process $ \txFsm -> do
    isStart <- txFsm === val False
    txCtr <- process $ \txCtr -> do
      ctrDone <- txCtr === val baud
      txCtr' <- C.inc txCtr
      ifm [ isStart `thenm` val 0
          , ctrDone `thenm` val 0
          , elsem txCtr'
          ]
    ctrDone <- txCtr === val baud
    notDone <- C.logicNot ctrDone
    txIx <- process $ \txIx -> do
      isEmpty <- txIx === val 9
      txIx'   <- C.inc txIx
      ifm [ notDone `thenm` txIx
          , isEmpty `thenm` val (0 :: Word8)
          , elsem txIx'
          ]
    isStartFrame <- txIx === val 0
    isEndFrame   <- txIx === val 9
    buf <- process $ \buf -> do
      buf' <- buf `C.shr` val True
      ifm [ isStart      `thenm` value byte
          , isStartFrame `thenm` buf
          , notDone      `thenm` buf
          , elsem buf'
          ]
    e <- buf `at` 0
    output "\\tx" =<< ifm
      [ isStart      `thenm` val True
      , isStartFrame `thenm` val False
      , isEndFrame   `thenm` val True
      , elsem e
      ]
    txFsm' <- C.logicNot =<< ctrDone `C.logicAnd` isEndFrame
    mux isStart txFsm' $ valid byte

  receive baud rx = do
    rxLow  <- rx === val False
    rxHigh <- C.logicNot rxLow
    (_, buf) <- machine $ \s -> do
      isIdle  <- rxFsm s === idle
      isStart <- rxFsm s === start
      isRecv  <- rxFsm s === recv
      isStop  <- rxFsm s === stop
      isBaudHalf        <- rxCtr s === val (baud `shiftR` 1)
      isBaudHalfRxStart <- isBaudHalf `C.logicAnd` isStart
      isBaud            <- rxCtr s === val baud
      isBaudRxRecv      <- isBaud `C.logicAnd` isRecv
      buf <- buffer $ OptSig isBaudRxRecv rx
      gotoRxStart <- rxLow `C.logicAnd` isIdle
      gotoRxRecv  <- rxLow `C.logicAnd` isBaudHalfRxStart
      gotoRxStop  <- C.logicAnd (valid buf) isRecv
      gotoRxIdle  <- do
        fromRxStart <- rxHigh `C.logicAnd` isBaudHalfRxStart
        fromRxStop  <- isBaud `C.logicAnd` isStop
        fromRxStart `C.logicOr` fromRxStop
      rxFsm' <- ifm
        [ gotoRxStart `thenm` start
        , gotoRxRecv  `thenm` recv
        , gotoRxStop  `thenm` stop
        , gotoRxIdle  `thenm` idle
        , elsem $ rxFsm s
        ]
      rxCtr1 <- C.inc $ rxCtr s
      rxCtr' <- ifm
        [ isIdle            `thenm` val 0
        , isBaudHalfRxStart `thenm` val 0
        , isBaud            `thenm` val 0
        , elsem rxCtr1
        ]
      return (Sig{ spec = pad <> spec rxCtr' <> spec rxFsm' }, packMaybe buf)
    return $ unpackMaybe buf
    where
      pad = "8'00000000"
      idle  = val 0
      start = val 1
      recv  = val 2
      stop  = val 3
      rxFsm :: Sig Word32 -> Sig Word8
      rxFsm = slice 7 0
      rxCtr :: Sig Word32 -> Sig Word16
      rxCtr = slice 23 8
      packMaybe :: OptSig (Array (Finite 8) Bool) -> Sig (Maybe Word8)
      packMaybe (OptSig validSig valueSig) = Sig $ spec validSig <> spec valueSig
      unpackMaybe :: Sig (Maybe Word8) -> OptSig Word8
      unpackMaybe s = OptSig (slice 8 8 s) (slice 7 0 s)

hello :: Monad m => MonadUart m => MonadSignal m => m (Sig Word32)
hello = process $ \timer -> do
  is5Sec <- timer === val 60000000
  transmit 624 $ OptSig is5Sec $ val 0x61
  flip (mux is5Sec) (val 0) =<< C.inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m ()
echo = transmit 624 =<< receive 624 =<< input "\\rx"
