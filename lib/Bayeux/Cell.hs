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
  , (.&&)
  , logicOr
  , (.||)
  , or
  , -- ** Shift
    shr
  , -- * Control
    ifm, thenm, elsem
  , patm, (~>), wildm
  ) where

import Bayeux.Rtl hiding (at, binary, mux, shift, shr, unary)
import Bayeux.Signal
import Control.Monad
import Data.Finitary
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe
import Prelude hiding (and, not, or)

-- | increment
inc :: Finitary a => MonadSignal m => Sig a -> m (Sig a)
inc a = binary addC a $ sig True

logicNot :: MonadSignal m => Sig Bool -> m (Sig Bool)
logicNot = unary logicNotC

not :: Finitary a => MonadSignal m => Sig a -> m (Sig a)
not = unary notC

add :: Finitary a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
add = binary addC

and :: Finitary a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
and = binary andC

eq :: Finitary a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
eq a = flip at 0 <=< eq' a
  where
    eq' :: Finitary a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    eq' = binary eqC

infix 4 ===
(===) :: Finitary a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
(===) = eq

liftBin :: Monad m => (Sig a -> Sig b -> m (Sig c)) -> m (Sig a) -> m (Sig b) -> m (Sig c)
liftBin f x y = do
  x' <- x
  y' <- y
  f x' y'

logicAnd :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
logicAnd = binary logicAndC

infixr 3 .&&
(.&&) :: Monad m => MonadSignal m => m (Sig Bool) -> m (Sig Bool) -> m (Sig Bool)
(.&&) = liftBin logicAnd

logicOr :: MonadSignal m => Sig Bool -> Sig Bool -> m (Sig Bool)
logicOr = binary logicOrC

infixr 2 .||
(.||) :: Monad m => MonadSignal m => m (Sig Bool) -> m (Sig Bool) -> m (Sig Bool)
(.||) = liftBin logicOr

or :: Finitary a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
or = binary orC

shr :: Finitary a => Finitary b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
shr = shift shrC

data Cond a = Cond
  { condition :: Maybe (Sig Bool)
  , result    :: Sig a
  }

ifm :: Finitary a => Monad m => MonadSignal m => NonEmpty (Cond a) -> m (Sig a)
ifm (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifm rest

thenm :: Sig Bool -> Sig a -> Cond a
thenm s = Cond (Just s)

elsem :: Sig a -> Cond a
elsem = Cond Nothing

data Pat p r = Pat
  { pat       :: Maybe p
  , patResult :: Sig r
  }

infix 5 ~>
(~>) :: p -> Sig r -> Pat p r
(~>) p = Pat (Just p)

wildm :: Sig r -> Pat p r
wildm = Pat Nothing

toCondition
  :: Finitary p
  => Monad m
  => MonadSignal m
  => Sig p
  -> Pat p r
  -> m (Cond r)
toCondition s p = case pat p of
  Nothing -> return $ Cond Nothing $ patResult p
  Just v  -> do
   isEq <- s === sig v
   return $ Cond
     { condition = Just isEq
     , result    = patResult p
     }

patm
  :: Finitary p
  => Finitary r
  => Monad m
  => MonadSignal m
  => Sig p
  -> NonEmpty (Pat p r)
  -> m (Sig r)
patm s = ifm <=< mapM (toCondition s)
