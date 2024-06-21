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
import Data.List.NonEmpty
import Data.Maybe

data Sig = Sig
  { spec   :: SigSpec
  , size   :: Integer
  , signed :: Bool
  }
  deriving (Eq, Read, Show)

class MonadSignal m where
  val :: Value -> m Sig
  input :: WireId -> m Sig

  process :: Bool    -- ^ signed
          -> Integer -- ^ width
          -> (Sig -> m Sig)
          -> m Sig
  at :: Sig -> Integer -> m Sig
  cat :: [Sig] -> m Sig

  -- | If S == 1 then B else A
  mux :: Sig   -- ^ S
      -> Sig   -- ^ A
      -> Sig   -- ^ B
      -> m Sig -- ^ Y

  unary :: ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Sig
        -> m Sig
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
         -> Sig
         -> Sig
         -> m Sig

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
        -> Sig
        -> Sig
        -> m Sig

instance MonadSignal Rtl where
  val v@(Value s _) = return $ Sig
    { spec = SigSpecConstant $ ConstantValue v
    , size = s
    , signed = False
    }

  input wireId = do
    i <- fresh
    tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput i] wireId]
    return Sig{ spec = SigSpecWireId wireId, size = 1, signed = False }

  process s w f = do
    old <- freshWire w
    let oldSig = Sig{ spec = old, size = w, signed = s }
    procStmt <- freshProcStmt
    srcSig <- f oldSig
    when (size srcSig /= w) $ throwError SizeMismatch
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return oldSig

  at s i = do
    when (i >= size s) $ throwError SizeMismatch
    t <- Rtl.at (spec s) i
    return Sig{ spec = t, size = 1, signed = False }

  cat sigs = do
    let sz = sum $ size <$> sigs
    y <- freshWire sz
    tell [ModuleBodyConnStmt $ ConnStmt y (SigSpecCat $ spec <$> sigs)]
    return Sig{ spec = y, size = sz, signed = False}

  mux s a b = do
    unless valid $ throwError SizeMismatch
    y <- Rtl.mux (size a) (spec s) (spec a) (spec b)
    return Sig{ spec = y, size = size a, signed = signed a }
    where
      valid =
        size s == 1 && not (signed s)
         && size a == size b && signed a == signed b

  unary cFn a = do
    y <- Rtl.unary cFn (signed a) (size a) (size a) (spec a)
    return Sig{ spec = y, size = size a, signed = signed a }

  binary cFn a b = do
    unless valid $ throwError SizeMismatch
    y <- Rtl.binary cFn (signed a) (size a) (signed b) (size b) (size a) (spec a) (spec b)
    return Sig{ spec = y, size = size a, signed = signed a }
    where
      valid = signed a == signed b && size a == size b

  shift cFn a b = do
    when (signed b) $ throwError SignedShift
    y <- Rtl.shift cFn (signed a) (size a) (size b) (size a) (spec a) (spec b)
    return Sig{ spec = y, size = size a, signed = signed a }

data Cond = Cond
  { condition :: Maybe Sig
  , result    :: Sig
  }

ifm :: Monad m => MonadSignal m => NonEmpty Cond -> m Sig
ifm (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifm rest

thenm :: Sig -> Sig -> Cond
thenm s = Cond (Just s)

elsem :: Sig -> Cond
elsem = Cond Nothing
