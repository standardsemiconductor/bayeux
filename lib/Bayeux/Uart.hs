{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  ) where

import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, unary)
import Bayeux.Signal
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Word

data OptSig = OptSig{ valid :: Sig, value :: Sig }
  deriving (Eq, Read, Show)

class MonadUart m where
  transmit :: Word16 -- ^ baud
           -> OptSig
           -> m ()

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
    txOut <- flip (mux isStartFrame) (zero 1) =<< flip (mux isEndFrame) one =<< buf `at` 0
    out "\\tx" =<< mux isStart txOut one
    txFsm' <- bar =<< ctrDone `conj` isEndFrame
    mux isStart txFsm' $ valid byte

shr :: MonadSignal m => Sig -> Sig -> m Sig
shr = shift shrC

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

hello :: Monad m => MonadUart m => MonadSignal m => m ()
hello = void $ process False 32 $ \timer -> do
  is5Sec <- eq timer =<< (val . binaryValue) (60000000 :: Word32)
  transmit 624 . OptSig is5Sec =<< (val . binaryValue) (0x61 :: Word8)
  flip (mux is5Sec) (zero 32) =<< inc timer
