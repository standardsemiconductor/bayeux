{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bayeux.Signal
  ( Sig(..)
  , val
  , OptSig(..)
  , MonadSignal(..)
  , ifm, thenm, elsem
  ) where

import Bayeux.Rtl hiding (mux)
import qualified Bayeux.Rtl as Rtl
import Bayeux.Width
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Binary
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe
import Data.String

newtype Sig a = Sig{ spec :: SigSpec }
  deriving (Eq, IsString, Read, Show)

instance Width a => Width (Sig a) where
  width _ = width (undefined :: a)

val :: Binary a => Width a => a -> Sig a
val v = let bs = foldMap binaryDigits $ LB.unpack $ encode v
        in Sig $ SigSpecConstant $ ConstantValue $ Value w $ drop (length bs - fromIntegral w) bs
  where
    w = width v

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

  binary cFn a b = Sig <$> Rtl.binary cFn False aSz False bSz ySz (spec a) (spec b)
    where
      aSz = width a
      bSz = width b
      ySz = aSz

  shift cFn a b = do
    Sig <$> Rtl.shift cFn False aSz bSz ySz (spec a) (spec b)
    where
      aSz = width a
      bSz = width b
      ySz = aSz

data Cond a = Cond
  { condition :: Maybe (Sig Bool)
  , result    :: Sig a
  }

ifm :: Width a => Monad m => MonadSignal m => NonEmpty (Cond a) -> m (Sig a)
ifm (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifm rest

thenm :: Sig Bool -> Sig a -> Cond a
thenm s = Cond (Just s)

elsem :: Sig a -> Cond a
elsem = Cond Nothing
