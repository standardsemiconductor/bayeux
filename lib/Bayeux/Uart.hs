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
    isStart <- C.eq txFsm $ val False
    txCtr <- process $ \txCtr -> do
      ctrDone <- C.eq txCtr $ val baud
      txCtr' <- C.inc txCtr
      ifm [ isStart `thenm` "16'0000000000000000"
          , ctrDone `thenm` "16'0000000000000000"
          , elsem txCtr'
          ]
    ctrDone <- C.eq txCtr $ val baud
    notDone <- C.logicNot ctrDone
    txIx <- process $ \txIx -> do
      isEmpty <- C.eq txIx nine
      txIx'   <- C.inc txIx
      ifm [ notDone `thenm` txIx
          , isEmpty `thenm` zero
          , elsem txIx'
          ]
    isStartFrame <- C.eq txIx zero
    isEndFrame   <- C.eq txIx nine
    buf <- process $ \buf -> do
      buf' <- shr buf
      ifm [ isStart      `thenm` value byte
          , isStartFrame `thenm` buf
          , notDone      `thenm` buf
          , elsem buf'
          ]
    e <- buf `at` 0
    out "\\tx" =<< ifm
      [ isStart      `thenm` val True
      , isStartFrame `thenm` val False
      , isEndFrame   `thenm` val True
      , elsem e
      ]
    txFsm' <- C.logicNot =<< ctrDone `C.logicAnd` isEndFrame
    mux isStart txFsm' $ valid byte

  receive baud rx = do
    rxLow  <- rx `C.eq` zero
    rxHigh <- C.logicNot rxLow
    s <- process $ \s -> do
      isIdle  <- rxFsm s `C.eq` idle
      isStart <- rxFsm s `C.eq` start
      isRecv  <- rxFsm s `C.eq` recv
      isStop  <- rxFsm s `C.eq` stop
      isBaudHalf        <- C.eq (rxCtr s) $ val (baud `shiftR` 1)
      isBaudHalfRxStart <- C.logicAnd isBaudHalf isStart
      isBaud            <- C.eq (rxCtr s) $ val baud
      isBaudRxRecv      <- C.logicAnd isBaud isRecv
      ixDone            <- C.eq (rxIx s) "8'00000111"
      gotoRxStart <- C.logicAnd rxLow isIdle
      gotoRxRecv  <- C.logicAnd rxLow isBaudHalfRxStart
      gotoRxStop  <- C.logicAnd isBaud =<< C.logicAnd ixDone isRecv
      gotoRxIdle  <- do
        fromRxStart <- C.logicAnd rxHigh isBaudHalfRxStart
        fromRxStop  <- C.logicAnd isBaud isStop
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
        [ isIdle            `thenm` "16'0000000000000000"
        , isBaudHalfRxStart `thenm` "16'0000000000000000"
        , isBaud            `thenm` "16'0000000000000000"
        , elsem rxCtr1
        ]
      rxIx1 <- C.inc $ rxIx s
      rxIx' <- ifm
        [ isIdle  `thenm` "8'00000000"
        , isStart `thenm` "8'00000000"
        , isStop  `thenm` "8'00000000"
        , isBaudRxRecv `thenm` rxIx1
        , elsem $ rxIx s
        ]
--      one8  <- val $ binaryValue (0x80 :: Word8)
--      zero8 <- val $ binaryValue (0x00 :: Word8)
--      mask  <- val $ binaryValue (0x7F :: Word8)
      rx8 <- ifm
        [ rx `thenm` "8'10000000" --0x80
        , elsem "8'00000000"
        ]
      shiftedBuf <- shr $ rxBuf s
      maskedBuf <- C.and shiftedBuf "8'01111111" --0x7F
      rxBufRx <- C.or rx8 maskedBuf
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
      idle  = "8'00000000"
      start = "8'00000001"
      recv  = "8'00000010"
      stop  = "8'00000011"
      rxFsm :: Sig Word64 -> Sig Word8
      rxFsm s = Sig{ spec = SigSpecSlice (spec s) 7  (Just 0) }
      rxCtr :: Sig Word64 -> Sig Word16
      rxCtr s = Sig{ spec = SigSpecSlice (spec s) 23 (Just 8) }
      rxIx :: Sig Word64 -> Sig Word8
      rxIx  s = Sig{ spec = SigSpecSlice (spec s) 31 (Just 24) }
      rxBuf :: Sig Word64 -> Sig Word8
      rxBuf s = Sig{ spec = SigSpecSlice (spec s) 39 (Just 32) }

shr :: FiniteBits a => MonadSignal m => Sig a -> m (Sig a)
shr a = shift shrC a one

zero :: forall a. FiniteBits a => Sig a
zero = Sig{ spec = SigSpecConstant $ ConstantValue $ Value (fromIntegral w) $ replicate w B0 }
  where
    w = finiteBitSize (undefined :: a)

one :: Sig Bool
one = Sig{ spec = SigSpecConstant $ ConstantValue $ Value 1 [B1] }

nine :: Sig Word8
nine = "8'00001001"

out :: WireId -> Sig Bool -> Rtl ()
out wireId sig = do
  i <- fresh
  tell
    [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput i] wireId
    , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) $ spec sig
    ]

hello :: Monad m => MonadUart m => MonadSignal m => m ()
hello = void $ process $ \timer -> do
  is5Sec <- C.eq timer $ val (60000000 :: Word32)
  transmit 624 $ OptSig is5Sec "8'01100001"
  flip (mux is5Sec) zero =<< C.inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m ()
echo = transmit 624 =<< receive 624 =<< input "\\rx"
