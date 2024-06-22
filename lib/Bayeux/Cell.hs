{-# LANGUAGE NoImplicitPrelude #-}

-- | Import this module qualified.
module Bayeux.Cell
  ( -- * Unary
    inc
  , logicNot
  , not
  , -- * Binary
    add
  , eq
  , logicAnd
  , logicOr
  , or
  ) where

import Bayeux.Rtl hiding (at, binary, unary)
import Bayeux.Signal
import Control.Monad
import Data.Bits
import Prelude hiding (not, or)

-- | increment
inc :: FiniteBits a => MonadSignal m => Sig a -> m (Sig a)
inc a = binary addC a $ val True

logicNot :: MonadSignal m => Sig Bool -> m (Sig Bool)
logicNot = unary logicNotC

not :: FiniteBits a => MonadSignal m => Sig a -> m (Sig a)
not = unary notC

add :: FiniteBits a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
add = binary addC

eq :: FiniteBits a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
eq a = flip at 0 <=< eq' a
  where
    eq' :: FiniteBits a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    eq' = binary eqC

logicAnd :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
logicAnd = binary logicAndC

logicOr :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
logicOr = binary logicOrC

or :: FiniteBits a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
or = binary orC
