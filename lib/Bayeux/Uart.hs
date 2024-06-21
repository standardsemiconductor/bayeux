{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  ) where

import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, shl, unary)
import Bayeux.Signal
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Bits hiding (shift)
import Data.Word

data OptSig = OptSig{ valid :: Sig, value :: Sig }
  deriving (Eq, Read, Show)

class MonadUart m where
  transmit :: Word16 -- ^ baud
           -> OptSig
           -> m ()
  receive :: Word16 -- ^ baud
          -> Sig    -- ^ rx
          -> m OptSig

instance MonadUart Rtl where
  transmit baud byte = void $ process False 1 $ \txFsm -> do
    isStart <- eq txFsm =<< (val . binaryValue) False
    txCtr <- process False 16 $ \txCtr -> do
      ctrDone <- eq txCtr =<< (val . binaryValue) baud
      txCtr' <- inc txCtr
      ifm [ isStart `thenm` zero 16
          , ctrDone `thenm` zero 16
          , elsem txCtr'
          ]
    ctrDone <- eq txCtr =<< (val . binaryValue) baud
    notDone <- bar ctrDone
    txIx <- process False 4 $ \txIx -> do
      isEmpty <- eq txIx nine
      txIx'   <- inc txIx
      ifm [ notDone `thenm` txIx
          , isEmpty `thenm` zero 4
          , elsem txIx'
          ]
    isStartFrame <- eq txIx $ zero 4
    isEndFrame   <- eq txIx nine
    buf <- process False 8 $ \buf -> do
      buf' <- shr buf one
      ifm [ isStart      `thenm` value byte
          , isStartFrame `thenm` buf
          , notDone      `thenm` buf
          , elsem buf'
          ]
    e <- buf `at` 0
    out "\\tx" =<< ifm
      [ isStart      `thenm` one
      , isStartFrame `thenm` zero 1
      , isEndFrame   `thenm` one
      , elsem e
      ]
    txFsm' <- bar =<< ctrDone `conj` isEndFrame
    mux isStart txFsm' $ valid byte

  receive baud rx = do
    rxLow  <- rx `eq` zero 1
    rxHigh <- bar rxLow
    idle   <- val $ Value 2 [B0, B0]
    start  <- val $ Value 2 [B0, B1]
    recv   <- val $ Value 2 [B1, B0]
    stop   <- val $ Value 2 [B1, B1]
    s <- process False 30 $ \s -> do
      isIdle  <- rxFsm s `eq` idle
      isStart <- rxFsm s `eq` start
      isRecv  <- rxFsm s `eq` recv
      isStop  <- rxFsm s `eq` stop
      isBaudHalf        <- eq (rxCtr s) =<< (val . binaryValue) (baud `shiftR` 1)
      isBaudHalfRxStart <- conj isBaudHalf isStart
      isBaud            <- eq (rxCtr s) =<< (val . binaryValue) baud
      isBaudRxRecv      <- conj isBaud isRecv
      ixDone            <- eq (rxIx s) =<< val (Value 4 [B0, B1, B1, B1])
      gotoRxStart <- conj rxLow isIdle
      gotoRxRecv  <- conj rxLow isBaudHalfRxStart
      gotoRxStop  <- conj isBaud =<< conj ixDone isRecv
      gotoRxIdle  <- do
        fromRxStart <- conj rxHigh isBaudHalfRxStart
        fromRxStop  <- conj isBaud isStop
        fromRxStart `disj` fromRxStop
      rxFsm' <- ifm
        [ gotoRxStart `thenm` start
        , gotoRxRecv  `thenm` recv
        , gotoRxStop  `thenm` stop
        , gotoRxIdle  `thenm` idle
        , elsem $ rxFsm s
        ]
      rxCtr1 <- inc $ rxCtr s
      rxCtr' <- ifm
        [ isIdle            `thenm` zero 16
        , isBaudHalfRxStart `thenm` zero 16
        , isBaud            `thenm` zero 16
        , elsem rxCtr1
        ]
      rxIx1 <- inc $ rxIx s
      rxIx' <- ifm
        [ isIdle  `thenm` zero 4
        , isStart `thenm` zero 4
        , isStop  `thenm` zero 4
        , isBaudRxRecv `thenm` rxIx1
        , elsem $ rxIx s
        ]
      one8  <- val $ binaryValue (0x80 :: Word8)
      zero8 <- val $ binaryValue (0x00 :: Word8)
      mask  <- val $ binaryValue (0x7F :: Word8)
      rx8 <- ifm
        [ rx `thenm` one8
        , elsem zero8
        ]
      shiftedBuf <- shr (rxBuf s) one
      maskedBuf <- conj shiftedBuf mask
      rxBufRx <- disj rx8 maskedBuf
{-
      maskedBuf <- conj (rxBuf s) mask
      rxBufRx  <- disj rx8 maskedBuf
      rxBufRx' <- shr rxBufRx one
-}
      rxBuf' <- ifm
        [ isBaudRxRecv `thenm` rxBufRx
        , elsem $ rxBuf s
        ]
      cat [rxBuf', rxIx', rxCtr', rxFsm']
--      return $ Sig{ spec   = mconcat $ map spec [rxBuf', rxIx', rxCtr', rxFsm']
--                  , size   = size s
--                  , signed = False
--                  }
    isStop  <- rxFsm s `eq` stop
    isBaud  <- eq (rxCtr s) =<< (val . binaryValue) baud
    isValid <- isStop `conj` isBaud
    return OptSig{ valid = isValid, value = rxBuf s }
    where
      rxFsm s = Sig{ spec = SigSpecSlice (spec s) 1  (Just 0),  size = 2,  signed = False }
      rxCtr s = Sig{ spec = SigSpecSlice (spec s) 17 (Just 2),  size = 16, signed = False }
      rxIx  s = Sig{ spec = SigSpecSlice (spec s) 21 (Just 18), size = 4,  signed = False }
      rxBuf s = Sig{ spec = SigSpecSlice (spec s) 29 (Just 22), size = 8,  signed = False }

shr :: MonadSignal m => Sig -> Sig -> m Sig
shr = shift shrC

shl :: MonadSignal m => Sig -> Sig -> m Sig
shl = shift shlC

eq :: Monad m => MonadSignal m => Sig -> Sig -> m Sig
eq a = flip at 0 <=< binary eqC a

zero :: Integer -> Sig
zero w =
  Sig{ spec = SigSpecConstant $ ConstantValue $ Value w $ replicate (fromIntegral w) B0
     , size = w
     , signed = False
     }

one :: Sig
one =
  Sig{ spec = SigSpecConstant $ ConstantValue $ Value 1 [B1]
     , size = 1
     , signed = False
     }

nine :: Sig
nine =
  Sig{ spec = SigSpecConstant $ ConstantValue $ Value 4 [B1, B0, B0, B1]
     , size = 4
     , signed = False
     }

inc :: Monad m => MonadSignal m => Sig -> m Sig
inc a = binary addC a =<< (val . Value (size a)) (replicate (fromIntegral (size a) - 1) B0 <> [B1])

bar :: Monad m => MonadSignal m => Sig -> m Sig
bar = flip at 0 <=< unary notC

out :: WireId -> Sig -> Rtl ()
out wireId sig = do
  when (size sig /= 1) $ throwError SizeMismatch
  i <- fresh
  tell
    [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput i] wireId
    , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) $ spec sig
    ]

conj :: MonadSignal m => Sig -> Sig -> m Sig
conj = binary andC

disj :: MonadSignal m => Sig -> Sig -> m Sig
disj = binary orC

hello :: Monad m => MonadUart m => MonadSignal m => m ()
hello = void $ process False 32 $ \timer -> do
  is5Sec <- eq timer =<< (val . binaryValue) (60000000 :: Word32)
  transmit 624 . OptSig is5Sec =<< (val . binaryValue) (0x61 :: Word8)
  flip (mux is5Sec) (zero 32) =<< inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m ()
echo = transmit 624 =<< receive 624 =<< input "\\rx"
