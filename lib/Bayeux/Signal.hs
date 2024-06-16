module Bayeux.Signal
  ( Sig(..)
  , MonadSignal(..)
  ) where

import Bayeux.Rtl
import qualified Bayeux.Rtl as Rtl
import Control.Monad
import Control.Monad.Writer
import Data.Bits

data Sig = Sig
  { spec   :: SigSpec
  , size   :: Integer
  , signed :: Bool
  }
  deriving (Eq, Read, Show)

class MonadSignal m where
  val :: FiniteBits a => a -> m Sig

  process :: Bool -> Integer -> (Sig -> m Sig) -> m Sig
  at :: Sig -> Integer -> m Sig

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
  val v = return $ Sig
    { spec = SigSpecConstant $ ConstantValue $ binaryValue v
    , size = fromIntegral $ finiteBitSize v
    , signed = False
    }

  process s w f = do
    old <- freshWire w
    let oldSig = Sig{ spec = old, size = w, signed = s }
    procStmt <- freshProcStmt
    srcSig <- f oldSig
    when (size srcSig /= w) $ error "size mismatch"
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return oldSig

  at s i
    | i >= size s = error "size mismatch"
    | otherwise   = do
        t <- Rtl.at (spec s) i
        return Sig{ spec = t, size = 1, signed = False }

  mux s a b
    | not valid = error "size mismatch"
    | otherwise = do
        y <- Rtl.mux (size a) (spec s) (spec a) (spec b)
        return Sig{ spec = y, size = size a, signed = signed a }
    where
      valid = and
        [ size s == 1 && signed s == False
        , size a == size b && signed a == signed b
        ]

  unary cFn a = do
    y <- Rtl.unary cFn (signed a) (size a) (size a) (spec a)
    return Sig{ spec = y, size = size a, signed = signed a }

  binary cFn a b
    | not valid = error "size mismatch"
    | otherwise = do
        y <- Rtl.binary cFn (signed a) (size a) (signed b) (size b) (size a) (spec a) (spec b)
        return Sig{ spec = y, size = size a, signed = signed a }
    where
      valid = signed a == signed b && size a == size b

  shift cFn a b
    | not valid = error "signed shift"
    | otherwise = do
        y <- Rtl.shift cFn (signed a) (size a) (size b) (size a) (spec a) (spec b)
        return Sig{ spec = y, size = size a, signed = signed a }
    where
      valid = not $ signed b
