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
  , -- * Control
    ifm, thenm, elsem
  , patm, (~>), wildm
  ) where

import Bayeux.Rtl hiding (at, binary, mux, shift, shr, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad
import Data.Binary
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe
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

data Pat p r = Pat
  { pattern       :: Maybe p
  , patternResult :: Sig r
  }

infix 5 ~>
(~>) :: p -> Sig r -> Pat p r
(~>) p = Pat (Just p)

wildm :: Sig r -> Pat p r
wildm = Pat Nothing

toCondition
  :: Binary p
  => Width p
  => Monad m
  => MonadSignal m
  => Sig p
  -> Pat p r
  -> m (Cond r)
toCondition s p = case pattern p of
  Nothing -> return $ Cond Nothing $ patternResult p
  Just v  -> do
   isEq <- s === val v
   return $ Cond
     { condition = Just isEq
     , result    = patternResult p
     }

patm
  :: Binary p
  => Width p
  => Width r
  => Monad m
  => MonadSignal m
  => Sig p
  -> NonEmpty (Pat p r)
  -> m (Sig r)
patm s = ifm <=< mapM (toCondition s)
