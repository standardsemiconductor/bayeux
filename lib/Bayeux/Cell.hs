{-# LANGUAGE NoImplicitPrelude #-}

-- | Import this module qualified.
module Bayeux.Cell
  ( -- * Unary
    inc
  , logicNot
  , not
  , -- * Binary
    add
  , and
  , eq
  , (===)
  , logicAnd
  , logicOr
  , or
  , -- ** Shift
    shr
  ) where

import Bayeux.Rtl hiding (at, binary, unary, shift, shr)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad
import Data.Bits hiding (shift)
import Prelude hiding (and, not, or)

-- | increment
inc :: Width a => MonadSignal m => Sig a -> m (Sig a)
inc a = binary addC a $ val True

logicNot :: MonadSignal m => Sig Bool -> m (Sig Bool)
logicNot = unary logicNotC

not :: Width a => MonadSignal m => Sig a -> m (Sig a)
not = unary notC

add :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
add = binary addC

and :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
and = binary andC

eq :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
eq a = flip at 0 <=< eq' a
  where
    eq' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    eq' = binary eqC

infix 4 ===
(===) :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
(===) = eq

logicAnd :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
logicAnd = binary logicAndC

logicOr :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
logicOr = binary logicOrC

or :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
or = binary orC

shr :: Width a => Width b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
shr = shift shrC
