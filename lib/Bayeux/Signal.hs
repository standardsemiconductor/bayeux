{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bayeux.Signal
  ( Sig(..)
  , val
  , slice
  , OptSig(..)
  , MonadSignal(..)
  ) where

import Bayeux.Encode
import Bayeux.Rtl hiding (mux)
import qualified Bayeux.Rtl as Rtl
import Bayeux.Width
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.String
import Prettyprinter hiding (width)

newtype Sig a = Sig{ spec :: SigSpec }
  deriving (Eq, IsString, Pretty, Read, Show)

instance Width a => Width (Sig a) where
  width _ = width (undefined :: a)

val :: Encode a => Width a => a -> Sig a
val v = Sig $ SigSpecConstant $ ConstantValue $ Value (width v) (encode v)

-- | Slice a signal. @slice 7 0@ is equal to @[7:0]@, the first byte.
slice
  :: Integer -- ^ end
  -> Integer -- ^ start
  -> Sig a
  -> Sig b
slice end start s = Sig{ spec = SigSpecSlice (spec s) end (Just start) }

data OptSig a = OptSig{ valid :: Sig Bool, value :: Sig a }
  deriving (Eq, Read, Show)

class MonadSignal m where
  input   :: WireId -> m (Sig Bool)
  output  :: WireId -> Sig Bool -> m ()
  process :: Width a => (Sig a -> m (Sig a)) -> m (Sig a)
  at      :: Width a => Sig a -> Integer -> m (Sig Bool)

  -- | If S == 1 then B else A
  mux :: Width a
      => Sig Bool   -- ^ S
      -> Sig a     -- ^ A
      -> Sig a     -- ^ B
      -> m (Sig a) -- ^ Y

  unary :: Width a
        => Width b
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
  binary :: Width a
         => Width b
         => Width c
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

  shift :: Width a
        => Width b
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

  process :: forall a. Width a => (Sig a -> Rtl (Sig a)) -> Rtl (Sig a)
  process f = do
    old <- freshWire $ width (undefined :: a)
    let oldSig = Sig old
    procStmt <- freshProcStmt
    srcSig <- f oldSig
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return oldSig

  at s i = do
    when (i >= sz) $ throwError SizeMismatch
    Sig <$> Rtl.at (spec s) i
    where
      sz = width s

  mux s a b = Sig <$> Rtl.mux sz (spec s) (spec a) (spec b)
    where
      sz = width a

  unary cFn a = Sig <$> Rtl.unary cFn False aSz ySz (spec a)
    where
      aSz = width a
      ySz = aSz

  binary
    :: forall a b c
     . Width a
    => Width b
    => Width c
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
    -> Rtl (Sig c)
  binary cFn a b = Sig <$> Rtl.binary cFn False aSz False bSz ySz (spec a) (spec b)
    where
      aSz = width a
      bSz = width b
      ySz = width (undefined :: c)

  shift cFn a b = do
    Sig <$> Rtl.shift cFn False aSz bSz ySz (spec a) (spec b)
    where
      aSz = width a
      bSz = width b
      ySz = aSz
