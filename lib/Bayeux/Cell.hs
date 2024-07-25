{-# LANGUAGE NoImplicitPrelude #-}

-- | Import this module qualified.
module Bayeux.Cell
  ( -- * Unary
    inc
  , dec
  , logicNot
  , not
  , -- * Binary
    and
  , or
  , xor
  , xnor
  , -- ** Shift
    shl
  , shr, sshr
  , -- ** Logical
    logicAnd
  , (.&&)
  , logicOr
  , (.||)
  , -- ** Compare
    eqx
  , nex
  , lt
  , le
  , eq
  , (===)
  , ne
  , ge
  , gt
  , -- ** Numeric
    add
  , sub
  , mul
  , div
  , mod
  , divFloor
  , modFloor
  , pow
  , -- * Control
    ifs, thens, elses
  , ifm, thenm, elsem
  , pats, (~~>), wilds
  , patm, (~>), wildm
  , -- * Data
    joinMaybe
  ) where

import Bayeux.Encode
import Bayeux.Rtl hiding (at, binary, mux, shift, shr, sshr, shl, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe
import Prelude hiding (and, div, mod, not, or)
import Yosys.Rtl

-- | increment
inc :: Encode a => Width a => MonadSignal m => Sig a -> m (Sig a)
inc a = binary addC a $ sig True

dec :: Encode a => Width a => MonadSignal m => Sig a -> m (Sig a)
dec a = binary subC a $ sig True

logicNot :: MonadSignal m => Sig Bool -> m (Sig Bool)
logicNot = unary logicNotC

not :: Width a => MonadSignal m => Sig a -> m (Sig a)
not = unary notC

and :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
and = binary andC

or :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
or = binary orC

xor :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
xor = binary xorC

xnor :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
xnor = binary xnorC

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

eqx :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
eqx a = flip at 0 <=< eqx' a
  where
    eqx' :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
    eqx' = binary eqxC

nex :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
nex a = flip at 0 <=< nex' a
  where
    nex' :: Width a => Monad m => MonadSignal m => Sig a -> Sig a -> m (Sig Bool)
    nex' = binary nexC

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

shr :: Width a => Width b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
shr = shift shrC

sshr :: Width a => Width b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
sshr = shift sshrC

shl :: Width a => Width b => MonadSignal m => Sig a -> Sig b -> m (Sig a)
shl = shift shlC

add :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
add = binary addC

sub :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
sub = binary subC

mul :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
mul = binary mulC

div :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
div = binary divC

mod :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
mod = binary modC

divFloor :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
divFloor = binary divFloorC

modFloor :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
modFloor = binary modFloorC

pow :: Width a => MonadSignal m => Sig a -> Sig a -> m (Sig a)
pow = binary powC

data Cond a = Cond
  { condition :: Maybe (Sig Bool)
  , result    :: Sig a
  }

ifs :: Width a => Monad m => MonadSignal m => NonEmpty (Cond a) -> m (Sig a)
ifs (a :| bs) = case nonEmpty bs of
  Nothing   -> return $ result a
  Just rest -> flip (mux (fromJust $ condition a)) (result a) =<< ifs rest

thens :: Sig Bool -> Sig a -> Cond a
thens s = Cond (Just s)

elses :: Sig a -> Cond a
elses = Cond Nothing

data CondM m a = CondM
  { conditionM :: Maybe (m (Sig Bool))
  , resultM    :: m (Sig a)
  }

ifm :: Width a => Monad m => MonadSignal m => NonEmpty (CondM m a) -> m (Sig a)
ifm (a :| bs) = do
  c <- fromMaybe (val True) $ conditionM a
  r <- resultM a
  case nonEmpty bs of
    Nothing -> return r
    Just rest -> flip (mux c) r =<< ifm rest

thenm :: m (Sig Bool) -> m (Sig a) -> CondM m a
thenm s = CondM (Just s)

elsem :: m (Sig a) -> CondM m a
elsem = CondM Nothing

data Pat p r = Pat
  { pat       :: Maybe p
  , patResult :: Sig r
  }

infix 5 ~~>
(~~>) :: p -> Sig r -> Pat p r
(~~>) p = Pat (Just p)

wilds :: Sig r -> Pat p r
wilds = Pat Nothing

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

data PatM p m r = PatM
  { patCond   :: Maybe p
  , runPat    :: m (Sig r)
  }

infix 5 ~>
(~>) :: p -> m (Sig r) -> PatM p m r
(~>) p = PatM (Just p)

wildm :: m (Sig r) -> PatM p m r
wildm = PatM Nothing

toConditionM
  :: Encode p
  => Width p
  => Monad m
  => MonadSignal m
  => Sig p
  -> PatM p m r
  -> m (Cond r)
toConditionM s p = do
  r <- runPat p
  case patCond p of
    Nothing -> return $ Cond Nothing r
    Just v  -> do
      isEq <- s === sig v
      return $ Cond
        { condition = Just isEq
        , result    = r
        }

patm
  :: Encode p
  => Width p
  => Width r
  => Monad m
  => MonadSignal m
  => Sig p
  -> NonEmpty (PatM p m r)
  -> m (Sig r)
patm s = ifs <=< mapM (toConditionM s)

joinMaybe :: Width a => Monad m => MonadSignal m => Sig (Maybe (Maybe a)) -> m (Sig (Maybe a))
joinMaybe s = do
  v <- sliceValid s `logicAnd` (sliceValid . sliceValue) s
  return $ Sig $ spec v <> (spec . sliceValue . sliceValue) s
