{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bayeux.Signal
  ( Sig(..)
  , val
  , MonadSignal(..)
  , ifm, thenm, elsem
  ) where

import Bayeux.Rtl hiding (mux)
import qualified Bayeux.Rtl as Rtl
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Bits
import Data.List.NonEmpty
import Data.Maybe
import Data.String

newtype Sig a = Sig{ spec :: SigSpec }
  deriving (Eq, IsString, Read, Show)

val :: FiniteBits a => a -> Sig a
val = Sig . SigSpecConstant . ConstantValue . binaryValue

instance Bits a => Bits (Sig a) where
  isSigned _ = isSigned (undefined :: a)

instance FiniteBits a => FiniteBits (Sig a) where
  finiteBitSize _ = finiteBitSize (undefined :: a)

class MonadSignal m where
  input   :: WireId -> m (Sig Bool)
  output  :: WireId -> Sig Bool -> m ()
  process :: FiniteBits a => (Sig a -> m (Sig a)) -> m (Sig a)
  at      :: FiniteBits a => Sig a -> Integer -> m (Sig Bool)
--  cat     :: [Sig a] -> m (Sig [a])

  -- | If S == 1 then B else A
  mux :: FiniteBits a
      => Sig Bool   -- ^ S
      -> Sig a     -- ^ A
      -> Sig a     -- ^ B
      -> m (Sig a) -- ^ Y

  unary :: FiniteBits a
        => FiniteBits b
        => ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Sig a
        -> m (Sig b)
  binary :: FiniteBits a
         => FiniteBits b
         => FiniteBits c
         => ( CellId
              -> Bool
              -> Integer
              -> Bool
              -> Integer
              -> Integer
              -> SigSpec
              -> SigSpec
              -> SigSpec
              -> Cell
            )
         -> Sig a
         -> Sig b
         -> m (Sig c)

  shift :: FiniteBits a
        => FiniteBits b
        => ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Sig a
        -> Sig b
        -> m (Sig a)

instance MonadSignal Rtl where
  input wireId = do
    i <- fresh
    tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput i] wireId]
    return $ Sig $ SigSpecWireId wireId

  output wireId sig = do
    i <- fresh
    tell
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput i] wireId
      , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) $ spec sig
      ]

  process :: forall a. FiniteBits a => (Sig a -> Rtl (Sig a)) -> Rtl (Sig a)
  process f = do
    let w = fromIntegral $ finiteBitSize (undefined :: a)
    old <- freshWire w
    let oldSig = Sig old
    procStmt <- freshProcStmt
    srcSig <- f oldSig
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return oldSig

  at s i = do
    when (i >= sz) $ throwError SizeMismatch
    Sig <$> Rtl.at (spec s) i
    where
      sz = fromIntegral $ finiteBitSize s
{-
  cat sigs = do
    let sz = sum $ size <$> sigs
    y <- freshWire sz
    tell [ModuleBodyConnStmt $ ConnStmt y (SigSpecCat $ spec <$> sigs)]
    return Sig{ spec = y, size = sz, signed = False}
-}
  mux s a b = Sig <$> Rtl.mux sz (spec s) (spec a) (spec b)
    where
      sz = fromIntegral $ finiteBitSize a

  unary cFn a = Sig <$> Rtl.unary cFn (isSigned a) aSz ySz (spec a)
    where
      aSz = fromIntegral $ finiteBitSize a
      ySz = aSz

  binary cFn a b = Sig <$> Rtl.binary cFn (isSigned a) aSz (isSigned b) bSz ySz (spec a) (spec b)
    where
      aSz = fromIntegral $ finiteBitSize a
      bSz = fromIntegral $ finiteBitSize b
      ySz = aSz

  shift cFn a b = do
    when (isSigned b) $ throwError SignedShift
    Sig <$> Rtl.shift cFn (isSigned a) aSz bSz ySz (spec a) (spec b)
    where
      aSz = fromIntegral $ finiteBitSize a
      bSz = fromIntegral $ finiteBitSize b
      ySz = aSz

data Cond a = Cond
  { condition :: Maybe (Sig Bool)
  , result    :: Sig a
  }

ifm :: FiniteBits a => Monad m => MonadSignal m => NonEmpty (Cond a) -> m (Sig a)
ifm (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifm rest

thenm :: Sig Bool -> Sig a -> Cond a
thenm s = Cond (Just s)

elsem :: Sig a -> Cond a
elsem = Cond Nothing
