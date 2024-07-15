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
  , lt
  , le
  , eq
  , (===)
  , ne
  , ge
  , gt
  , logicAnd
  , (.&&)
  , logicOr
  , (.||)
  , or
  , -- ** Shift
    shr
  , -- * Control
    ifs, thenm, elsem
  , pats, (~>), wildm
  ) where

import Bayeux.Encode
import Bayeux.Rtl hiding (at, binary, mux, shift, shr, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe
import Prelude hiding (and, not, or)

-- | increment
inc :: Encode a => Width a => MonadSignal m => Sig a -> m (Sig a)
inc a = binary addC a $ sig True

logicNot :: MonadSignal m => Sig Bool -> m (Sig Bool)
logicNot = unary logicNotC

not :: Width a => MonadSignal m => Sig a -> m (Sig a)
not = unary notC

add :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
add = binary addC

and :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
and = binary andC

lt :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
lt a = flip at 0 <=< lt' a
  where
    lt' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    lt' = binary ltC

le :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
le a = flip at 0 <=< le' a
  where
    le' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    le' = binary leC

eq :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
eq a = flip at 0 <=< eq' a
  where
    eq' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    eq' = binary eqC

infix 4 ===
(===) :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
(===) = eq

ne :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
ne a = flip at 0 <=< ne' a
  where
    ne' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    ne' = binary neC

ge :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
ge a = flip at 0 <=< ge' a
  where
    ge' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    ge' = binary geC

gt :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
gt a = flip at 0 <=< gt' a
  where
    gt' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
    gt' = binary gtC

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

or :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
or = binary orC

shr :: Width a => Width b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
shr = shift shrC

data Cond a = Cond
  { condition :: Maybe (Sig Bool)
  , result    :: Sig a
  }

ifs :: Width a => Monad m => MonadSignal m => NonEmpty (Cond a) -> m (Sig a)
ifs (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifs rest

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
  :: Encode p
  => Width p
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

pats
  :: Encode p
  => Width p
  => Width r
  => Monad m
  => MonadSignal m
  => Sig p
  -> NonEmpty (Pat p r)
  -> m (Sig r)
pats s = ifs <=< mapM (toCondition s)
