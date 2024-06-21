module Bayeux.Signal
  ( Sig(..)
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

newtype Sig a = Sig{ spec :: SigSpec }
  deriving (Eq, Read, Show)

class MonadSignal m where
  val     :: FiniteBits a => a -> m (Sig a)
  input   :: WireId -> m (Sig Bool)
  process :: (Sig a -> m (Sig a)) -> m (Sig a)
  at      :: Sig a -> Integer -> m (Sig Bool)
--  cat     :: [Sig a] -> m (Sig [a])

  -- | If S == 1 then B else A
  mux :: Sig Bool   -- ^ S
      -> Sig a     -- ^ A
      -> Sig a     -- ^ B
      -> m (Sig a) -- ^ Y

  unary :: ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Sig a
        -> m (Sig b)
  binary :: ( CellId
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

  shift :: ( CellId
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
  val = return . Sig . SigSpecConstant . ConstantValue . binaryValue

  input wireId = do
    i <- fresh
    tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput i] wireId]
    return $ Sig $ SigSpecWireId wireId

  process f = do
    old <- freshWire w -- freshSig?
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
    when (isSigned b) $ throwError SizeMismatch -- TODO fix
    Sig <$> Rtl.shift cFn (isSigned a) aSz bSz ySz (spec a) (spec b)
    where
      aSz = fromIntegral $ finiteBitSize a
      bSz = fromIntegral $ finiteBitSize b
      ySz = aSz

data Cond a = Cond
  { condition :: Maybe (Sig Bool)
  , result    :: Sig a
  }

ifm :: Monad m => MonadSignal m => NonEmpty (Cond a) -> m (Sig a)
ifm (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifm rest

thenm :: Sig Bool -> Sig a -> Cond a
thenm s = Cond (Just s)

elsem :: Sig a -> Cond a
elsem = Cond Nothing
