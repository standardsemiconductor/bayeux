{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bayeux.Signal
  ( Sig(..)
  , sig
  , slice
  , toMaybeSig
  , fromMaybeSig
  , sliceValid
  , sliceValue
  , sliceFst
  , sliceSnd
  , MonadSignal(..)
  ) where

import Bayeux.Rtl hiding (mux)
import qualified Bayeux.Rtl as Rtl
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Finitary
import Data.String
import Prettyprinter hiding (width)

newtype Sig a = Sig{ spec :: SigSpec }
  deriving (Eq, IsString, Pretty, Read, Show)

sig :: Finitary a => a -> Sig a
sig v = Sig $ SigSpecConstant $ ConstantValue $ Value (width v) (encode v)

-- | Slice a signal. @slice 7 0@ is equal to @[7:0]@, the first byte.
slice
  :: Integer -- ^ end
  -> Integer -- ^ begin
  -> Sig a
  -> Sig b
slice e b s = Sig{ spec = SigSpecSlice (spec s) e (Just b) }

toMaybeSig :: Sig Bool -> Sig a -> Sig (Maybe a)
toMaybeSig validSig valueSig = Sig $ spec validSig <> spec valueSig

fromMaybeSig :: forall a. Finitary a => Sig (Maybe a) -> (Sig Bool, Sig a)
fromMaybeSig s = (slice (w - 1) (w - 1) s, slice (w - 2) 0 s)
  where
    w = width (undefined :: a)

sliceValid :: Finitary a => Sig (Maybe a) -> Sig Bool
sliceValid = fst . fromMaybeSig

sliceValue :: Finitary a => Sig (Maybe a) -> Sig a
sliceValue = snd . fromMaybeSig

sliceFst :: forall a b. Finitary a => Finitary b => Sig (a, b) -> Sig a
sliceFst = slice (width (undefined :: (a, b)) - 1) (width (undefined :: b))

sliceSnd :: forall a b. Finitary b => Sig (a, b) -> Sig b
sliceSnd = slice (width (undefined :: b) - 1) 0

class MonadSignal m where
  input   :: WireId -> m (Sig Bool)
  output  :: WireId -> Sig Bool -> m ()
  value   :: Finitary a => a -> m (Sig a)
  process :: Finitary a => (Sig a -> m (Sig a)) -> m (Sig a)
  machine :: Finitary s
          => Finitary o
          => (Sig s -> m (Sig s, Sig o))
          -> m (Sig s, Sig o)
  at      :: Finitary a => Sig a -> Integer -> m (Sig Bool)

  -- | If S == 1 then B else A
  mux :: Finitary a
      => Sig Bool   -- ^ S
      -> Sig a     -- ^ A
      -> Sig a     -- ^ B
      -> m (Sig a) -- ^ Y

  unary :: Finitary a
        => Finitary b
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
  binary :: Finitary a
         => Finitary b
         => Finitary c
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

  shift :: Finitary a
        => Finitary b
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

  output wireId s = do
    i <- fresh
    tell
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput i] wireId
      , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) $ spec s
      ]

  value = return . sig
  
  process :: forall a. Finitary a => (Sig a -> Rtl (Sig a)) -> Rtl (Sig a)
  process f = do
    old <- freshWire $ width (undefined :: a)
    let oldSig = Sig old
    procStmt <- freshProcStmt
    srcSig <- f oldSig
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return oldSig

  machine
    :: forall s o
     . Finitary s
    => Finitary o
    => (Sig s -> Rtl (Sig s, Sig o))
    -> Rtl (Sig s, Sig o)
  machine f = do
    old <- freshWire $ width (undefined :: s)
    let oldSig = Sig old
    procStmt <- freshProcStmt
    (srcSig, outSig) <- f oldSig
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return (oldSig, outSig)

  at :: forall a. Finitary a => Sig a -> Integer -> Rtl (Sig Bool)
  at s i = do
    when (i >= sz) $ throwError SizeMismatch
    Sig <$> Rtl.at (spec s) i
    where
      sz = width (undefined :: a)

  mux :: forall a
       . Finitary a
      => Sig Bool    -- ^ S
      -> Sig a       -- ^ A
      -> Sig a       -- ^ B
      -> Rtl (Sig a) -- ^ Y
  mux s a b = Sig <$> Rtl.mux sz (spec s) (spec a) (spec b)
    where
      sz = width (undefined :: a)

  unary :: forall a b
         . Finitary a
        => Finitary b
        => ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Sig a
        -> Rtl (Sig b)
  unary cFn a = Sig <$> Rtl.unary cFn False aSz ySz (spec a)
    where
      aSz = width (undefined :: a)
      ySz = aSz

  binary
    :: forall a b c
     . Finitary a
    => Finitary b
    => Finitary c
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
      aSz = width (undefined :: a)
      bSz = width (undefined :: b)
      ySz = width (undefined :: c)

  shift
    :: forall a b
     . Finitary a
    => Finitary b
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
    -> Rtl (Sig a)
  shift cFn a b = do
    Sig <$> Rtl.shift cFn False aSz bSz ySz (spec a) (spec b)
    where
      aSz = width (undefined :: a)
      bSz = width (undefined :: b)
      ySz = aSz
