{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  ) where

import qualified Bayeux.Cell as C
import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, unary)
import Bayeux.Signal
import Control.Monad
import Data.Bits hiding (shift)
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
    isStart <- C.eq txFsm $ val False
    txCtr <- process $ \txCtr -> do
      ctrDone <- C.eq txCtr $ val baud
      txCtr' <- C.inc txCtr
      ifm [ isStart `thenm` val 0
          , ctrDone `thenm` val 0
          , elsem txCtr'
          ]
    ctrDone <- C.eq txCtr $ val baud
    notDone <- C.logicNot ctrDone
    txIx <- process $ \txIx -> do
      isEmpty <- txIx `C.eq` val 9
      txIx'   <- C.inc txIx
      ifm [ notDone `thenm` txIx
          , isEmpty `thenm` val (0 :: Word8)
          , elsem txIx'
          ]
    isStartFrame <- txIx `C.eq` val 0
    isEndFrame   <- txIx `C.eq` val 9
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
    rxLow  <- rx `C.eq` val False
    rxHigh <- C.logicNot rxLow
    s <- process $ \s -> do
      isIdle  <- rxFsm s `C.eq` idle
      isStart <- rxFsm s `C.eq` start
      isRecv  <- rxFsm s `C.eq` recv
      isStop  <- rxFsm s `C.eq` stop
      isBaudHalf        <- rxCtr s `C.eq` val (baud `shiftR` 1)
      isBaudHalfRxStart <- isBaudHalf `C.logicAnd` isStart
      isBaud            <- rxCtr s `C.eq` val baud
      isBaudRxRecv      <- isBaud `C.logicAnd` isRecv
      ixDone            <- rxIx s `C.eq` val 7
      gotoRxStart <- rxLow `C.logicAnd` isIdle
      gotoRxRecv  <- rxLow `C.logicAnd` isBaudHalfRxStart
      gotoRxStop  <- C.logicAnd isBaud =<< C.logicAnd ixDone isRecv
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
      rxIx1 <- C.inc $ rxIx s
      rxIx' <- ifm
        [ isIdle  `thenm` val 0
        , isStart `thenm` val 0
        , isStop  `thenm` val 0
        , isBaudRxRecv `thenm` rxIx1
        , elsem $ rxIx s
        ]
      rx8 <- ifm
        [ rx `thenm` val 0x80
        , elsem $    val 0x00
        ]
      shiftedBuf <- rxBuf s `C.shr` val True
      maskedBuf <- shiftedBuf `C.and` val 0x7F
      rxBufRx <- rx8 `C.or` maskedBuf
      rxBuf' <- ifm
        [ isBaudRxRecv `thenm` rxBufRx
        , elsem $ rxBuf s
        ]
      return Sig{ spec = pad <> spec rxBuf' <> spec rxIx' <> spec rxCtr' <> spec rxFsm' }
    isStop  <- rxFsm s `C.eq` stop
    isBaud  <- C.eq (rxCtr s) $ val baud
    isValid <- isStop `C.logicAnd` isBaud
    return OptSig{ valid = isValid, value = rxBuf s }
    where
      pad = "24'000000000000000000000000"
      idle  = val 0
      start = val 1
      recv  = val 2
      stop  = val 3
      rxFsm :: Sig Word64 -> Sig Word8
      rxFsm s = Sig{ spec = SigSpecSlice (spec s) 7  (Just 0) }
      rxCtr :: Sig Word64 -> Sig Word16
      rxCtr s = Sig{ spec = SigSpecSlice (spec s) 23 (Just 8) }
      rxIx :: Sig Word64 -> Sig Word8
      rxIx  s = Sig{ spec = SigSpecSlice (spec s) 31 (Just 24) }
      rxBuf :: Sig Word64 -> Sig Word8
      rxBuf s = Sig{ spec = SigSpecSlice (spec s) 39 (Just 32) }

hello :: Monad m => MonadUart m => MonadSignal m => m (Sig Word32)
hello = process $ \timer -> do
  is5Sec <- timer `C.eq` val 60000000
  transmit 624 $ OptSig is5Sec $ val 0x61
  flip (mux is5Sec) (val 0) =<< C.inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m ()
echo = transmit 624 =<< receive 624 =<< input "\\rx"
