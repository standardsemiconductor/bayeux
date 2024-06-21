{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  ) where

import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, unary)
import Bayeux.Signal
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Bits hiding (shift)
import Data.Word

data OptSig a = OptSig{ valid :: Sig Bool, value :: Sig a }
  deriving (Eq, Read, Show)

class MonadUart m where
  transmit :: Word16 -- ^ baud
           -> OptSig Word8
           -> m ()
  receive :: Word16   -- ^ baud
          -> Sig Bool -- ^ rx
          -> m (OptSig Word8)

instance MonadUart Rtl where
  transmit baud byte = void $ process $ \txFsm -> do
    isStart <- eq txFsm =<< val False
    txCtr <- process $ \txCtr -> do
      ctrDone <- eq txCtr =<< val baud
      txCtr' <- inc txCtr
      ifm [ isStart `thenm` 0
          , ctrDone `thenm` 0
          , elsem txCtr'
          ]
    ctrDone <- eq txCtr =<< val baud
    notDone <- bar ctrDone
    txIx <- process $ \txIx -> do
      isEmpty <- eq txIx nine
      txIx'   <- inc txIx
      ifm [ notDone `thenm` txIx
          , isEmpty `thenm` zero
          , elsem txIx'
          ]
    isStartFrame <- eq txIx zero
    isEndFrame   <- eq txIx nine
    buf <- process $ \buf -> do
      buf' <- shr buf one
      ifm [ isStart      `thenm` value byte
          , isStartFrame `thenm` buf
          , notDone      `thenm` buf
          , elsem buf'
          ]
    e <- buf `at` 0
    t <- val True
    f <- val False
    out "\\tx" =<< ifm
      [ isStart      `thenm` t
      , isStartFrame `thenm` f
      , isEndFrame   `thenm` t
      , elsem e
      ]
    txFsm' <- bar =<< ctrDone `conj` isEndFrame
    mux isStart txFsm' $ valid byte

  receive baud rx = do
    rxLow  <- rx `eq` zero
    rxHigh <- bar rxLow
    s <- process $ \s -> do
      isIdle  <- rxFsm s `eq` idle
      isStart <- rxFsm s `eq` start
      isRecv  <- rxFsm s `eq` recv
      isStop  <- rxFsm s `eq` stop
      isBaudHalf        <- eq (rxCtr s) =<< val (baud `shiftR` 1)
      isBaudHalfRxStart <- conj isBaudHalf isStart
      isBaud            <- eq (rxCtr s) =<< val baud
      isBaudRxRecv      <- conj isBaud isRecv
      ixDone            <- eq (rxIx s) 7
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
        [ isIdle            `thenm` 0
        , isBaudHalfRxStart `thenm` 0
        , isBaud            `thenm` 0
        , elsem rxCtr1
        ]
      rxIx1 <- inc $ rxIx s
      rxIx' <- ifm
        [ isIdle  `thenm` 0
        , isStart `thenm` 0
        , isStop  `thenm` 0
        , isBaudRxRecv `thenm` rxIx1
        , elsem $ rxIx s
        ]
--      one8  <- val $ binaryValue (0x80 :: Word8)
--      zero8 <- val $ binaryValue (0x00 :: Word8)
--      mask  <- val $ binaryValue (0x7F :: Word8)
      rx8 <- ifm
        [ rx `thenm` 0x80
        , elsem 0x00
        ]
      shiftedBuf <- shr (rxBuf s) one
      maskedBuf <- bitwiseAnd shiftedBuf 0x7F
      rxBufRx <- bitwiseOr rx8 maskedBuf
      rxBuf' <- ifm
        [ isBaudRxRecv `thenm` rxBufRx
        , elsem $ rxBuf s
        ]
      return Sig{ spec = spec rxBuf' <> spec rxIx' <> spec rxCtr' <> spec rxFsm' }
    isStop  <- rxFsm s `eq` stop
    isBaud  <- eq (rxCtr s) =<< val baud
    isValid <- isStop `conj` isBaud
    return OptSig{ valid = isValid, value = rxBuf s }
    where
      idle  = 0
      start = 1
      recv  = 2
      stop  = 3
      rxFsm :: Sig Word64 -> Sig Word8
      rxFsm s = Sig{ spec = SigSpecSlice (spec s) 1  (Just 0) }
      rxCtr :: Sig Word64 -> Sig Word16
      rxCtr s = Sig{ spec = SigSpecSlice (spec s) 17 (Just 2) }
      rxIx :: Sig Word64 -> Sig Word8
      rxIx  s = Sig{ spec = SigSpecSlice (spec s) 21 (Just 18) }
      rxBuf :: Sig Word64 -> Sig Word8
      rxBuf s = Sig{ spec = SigSpecSlice (spec s) 29 (Just 22) }

shr :: FiniteBits a => FiniteBits b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
shr = shift shrC

eq :: FiniteBits a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
eq a = binary eqC a

zero :: forall a. FiniteBits a => Sig a
zero = Sig{ spec = SigSpecConstant $ ConstantValue $ Value (fromIntegral w) $ replicate w B0 }
  where
    w = finiteBitSize (undefined :: a)

one :: Sig Bool
one = Sig{ spec = SigSpecConstant $ ConstantValue $ Value 1 [B1] }

nine :: Sig Word8
nine = 9

inc :: FiniteBits a => Monad m => MonadSignal m => Sig a -> m (Sig a)
inc a = binary addC a =<< val True

bar :: Monad m => MonadSignal m => Sig Bool -> m (Sig Bool)
bar = unary notC

out :: WireId -> Sig Bool -> Rtl ()
out wireId sig = do
  i <- fresh
  tell
    [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput i] wireId
    , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) $ spec sig
    ]

bitwiseAnd :: FiniteBits a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
bitwiseAnd = binary andC

conj :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
conj = bitwiseAnd

bitwiseOr :: FiniteBits a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
bitwiseOr = binary orC

disj :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
disj = bitwiseOr

hello :: Monad m => MonadUart m => MonadSignal m => m ()
hello = void $ process $ \timer -> do
  is5Sec <- eq timer =<< val (60000000 :: Word32)
  transmit 624 $ OptSig is5Sec 0x61
  flip (mux is5Sec) zero =<< inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m ()
echo = transmit 624 =<< receive 624 =<< input "\\rx"
