{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Uart
  ( echo
  , hello
  ) where

import Bayeux.Rtlil
import Control.Monad
import Control.Monad.Writer
import Data.Char
import Data.Word

data OptSig = OptSig{ valid :: SigSpec, value :: SigSpec }
  deriving (Eq, Read, Show)

class MonadUart m where
  receive  :: m OptSig
  transmit :: Word16 -- ^ baud
           -> OptSig
           -> m ()

instance MonadUart Rtl where
  receive = undefined
  transmit baud byte = void $ process 1 $ \txFsm -> do
    isStart <- eq 1 txFsm $ SigSpecConstant $ ConstantValue $ binaryValue False--zero
    txCtr <- process 16 $ \txCtr -> do
      ctrDone <- eq 16 txCtr $ SigSpecConstant $ ConstantValue $ binaryValue baud
      flip (mux 16 isStart) (SigSpecConstant $ ConstantValue $ binaryValue (0 :: Word16)) =<< flip (mux 16 ctrDone) (SigSpecConstant $ ConstantValue $ binaryValue (0 :: Word16)) =<< inc 16 txCtr
    ctrDone <- eq 16 txCtr $ SigSpecConstant $ ConstantValue $ binaryValue baud
    txIx <- process 4 $ \txIx -> do
      isEmpty <- eq 4 txIx $ SigSpecConstant $ ConstantValue $ Value 4 [B1, B0, B0, B1] -- nine
      mux 4 ctrDone txIx =<< flip (mux 4 isEmpty) (SigSpecConstant $ ConstantValue $ Value 4 [B0, B0, B0, B0]) =<< inc 4 txIx
    isStartFrame <- eq 4 txIx $ SigSpecConstant $ ConstantValue $ Value 4 [B0, B0, B0, B0]
    isEndFrame   <- eq 4 txIx $ SigSpecConstant $ ConstantValue $ Value 4 [B1, B0, B0, B1]
    buf <- process 8 $ \buf -> do
      buf' <- mux 8 ctrDone buf =<< shl False 8 1 8 buf (SigSpecConstant $ ConstantValue $ Value 1 [B1])
      mux 8 isStart buf' $ value byte
    txOut <- flip (mux 1 isStartFrame) (SigSpecConstant $ ConstantValue $ Value 1 [B0]) =<< flip (mux 1 isEndFrame) (SigSpecConstant $ ConstantValue $ Value 1 [B1]) =<< buf `at` 0
    out "\\tx" =<< mux 1 isStart txOut (SigSpecConstant $ ConstantValue $ Value 1 [B1])
    txFsm' <- bar =<< ctrDone `conj` isEndFrame
    mux 1 isStart txFsm' $ valid byte

eq :: MonadRtl m => Integer -> SigSpec -> SigSpec -> m SigSpec
eq w = binary eqC False w False w 1

zero :: SigSpec
zero = SigSpecConstant $ ConstantInteger 0

one :: SigSpec
one = SigSpecConstant $ ConstantInteger 1

nine :: SigSpec
nine = SigSpecConstant $ ConstantInteger 9

inc :: MonadRtl m => Integer -> SigSpec -> m SigSpec
inc bWidth = binary addC False bWidth False bWidth bWidth (SigSpecConstant $ ConstantValue $ Value bWidth $ replicate (fromEnum bWidth - 1) B0 <> [B1])

bar :: MonadRtl m => SigSpec -> m SigSpec
bar = unary notC False 1 1

out :: WireId -> SigSpec -> Rtl ()
out wireId sig = do
  i <- fresh
  tell
    [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 1] wireId
    , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) sig
    ]

conj :: MonadRtl m => SigSpec -> SigSpec -> m SigSpec
conj = binary andC False 1 False 1 1

echo :: Monad m => MonadUart m => m ()
echo = transmit 624 =<< receive

hello :: Monad m => MonadUart m => MonadRtl m => m ()
hello = void $ process 32 $ \timer -> do
  is5Sec <- eq 32 timer $ SigSpecConstant $ ConstantInteger 60000000
  transmit 624 $ OptSig is5Sec $ SigSpecConstant $ ConstantValue $ Value 8 [B0, B1, B1, B0, B0, B0, B0, B1]
  flip (mux 32 is5Sec) zero =<< inc 32 timer
