{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Bayeux.Signal
  ( Sig(..)
  , sig
  , slice
  , toMaybeSig
  , justSig
  , fromMaybeSig
  , sliceValid
  , sliceValue
  , mapMaybeSig
  , sliceFst
  , sliceSnd
  , sliceIx
  , sliceRange
  , sliceReverse
  , sliceRotate
  , MonadSignal(..)
  ) where

import Bayeux.Encode
import Bayeux.Rtl hiding (mux)
import qualified Bayeux.Rtl as Rtl
import Bayeux.Width
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Array
import Data.Finite
import Data.Proxy
import Data.String
import GHC.TypeLits
import Prettyprinter hiding (width)
import Yosys.Rtl

newtype Sig a = Sig{ spec :: SigSpec }
  deriving (Eq, IsString, Pretty, Read, Show)

instance Width a => Width (Sig a) where
  width _ = width (undefined :: a)

sig :: Encode a => Width a => a -> Sig a
sig v = Sig $ SigSpecConstant $ ConstantValue $ Value (width v) (encode v)

-- | Slice a signal. @slice 7 0@ is equal to @[7:0]@, the first byte.
slice
  :: Integer -- ^ end
  -> Integer -- ^ start
  -> Sig a
  -> Sig b
slice end start s = Sig{ spec = SigSpecSlice (spec s) end (Just start) }

toMaybeSig :: Sig Bool -> Sig a -> Sig (Maybe a)
toMaybeSig validSig valueSig = Sig $ spec validSig <> spec valueSig

justSig :: Sig a -> Sig (Maybe a)
justSig = toMaybeSig $ sig True

fromMaybeSig :: Width a => Sig (Maybe a) -> (Sig Bool, Sig a)
fromMaybeSig s = (slice (w - 1) (w - 1) s, slice (w - 2) 0 s)
  where
    w = width s

sliceValid :: Width a => Sig (Maybe a) -> Sig Bool
sliceValid = fst . fromMaybeSig

sliceValue :: Width a => Sig (Maybe a) -> Sig a
sliceValue = snd . fromMaybeSig

mapMaybeSig :: Width a => (Sig a -> Sig b) -> Sig (Maybe a) -> Sig (Maybe b)
mapMaybeSig f a = Sig $ (spec . sliceValid) a <> (spec . f . sliceValue) a

sliceFst :: forall a b. Width a => Width b => Sig (a, b) -> Sig a
sliceFst s = slice (width s - 1) (width (undefined :: b)) s

sliceSnd :: forall a b. Width b => Sig (a, b) -> Sig b
sliceSnd = slice (width (undefined :: b) - 1) 0

-- | Slice element @i@ from an 'Array'.
sliceIx
  :: forall n e
   . KnownNat n
  => Width e
  => Finite n
  -> Sig (Array (Finite n) e)
  -> Sig e
sliceIx i = slice ((getFinite i + 1)*w - 1) (getFinite i * w)
  where
    w = width (undefined :: e)

-- | Slice a range of elements from 'Sig' 'Array' inclusive. Indexing starts at
-- zero from the right.
sliceRange
  :: forall n m e
   . KnownNat n
  => KnownNat m
  => Width e
  => Finite n -- ^ end
  -> Finite n -- ^ begin
  -> Sig (Array (Finite n) e)
  -> Sig (Array (Finite m) e)
sliceRange e b arr = Sig $ flip foldMap (reverse [b..e]) $ \i -> spec $ sliceIx i arr

-- | Reverse the elements of a 'Sig' 'Array'.
sliceReverse :: forall n e. KnownNat n => Width e => Sig (Array (Finite n) e) -> Sig (Array (Finite n) e)
sliceReverse arr = Sig $ flip foldMap [0..n - 1] $ \i -> spec $ sliceIx i arr
  where
    n = finite $ natVal (Proxy :: Proxy n)

sliceRotate
  :: forall n e
   . KnownNat n
  => Width e
  => Int -- ^ Negative values rotate left, positive right.
  -> Sig (Array (Finite n) e)
  -> Sig (Array (Finite n) e)
sliceRotate m arr
  | m < 0     = Sig $ foldMap spec $ rotateL m' slices
  | otherwise = Sig $ foldMap spec $ rotateR m  slices
  where
    m' = abs m
    ixs :: [Finite n]
    ixs = finite . fromIntegral <$> reverse [0..natVal (Proxy :: Proxy n) - 1]
    slices :: [Sig e]
    slices = flip sliceIx arr <$> ixs

rotateL :: Int -> [a] -> [a]
rotateL n xs = take (length xs) $ drop (length xs + n) $ cycle xs

rotateR :: Int -> [a] -> [a]
rotateR n xs = rotateL (length xs - n) xs

class MonadSignal m where
  input   :: WireId -> m (Sig Bool)
  output  :: WireId -> Sig Bool -> m ()
  val     :: Encode a => Width a => a -> m (Sig a)
  process :: Width a => (Sig a -> m (Sig a)) -> m (Sig a)
  machine :: Width s => Width o => (Sig s -> m (Sig s, Sig o)) -> m (Sig s, Sig o)
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

  output wireId s = do
    i <- fresh
    tell
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput i] wireId
      , ModuleBodyConnStmt $ ConnStmt (SigSpecWireId wireId) $ spec s
      ]

  val = return . sig

  process :: forall a. Width a => (Sig a -> Rtl (Sig a)) -> Rtl (Sig a)
  process f = do
    old <- freshWire $ width (undefined :: a)
    let oldSig = Sig old
    procStmt <- freshProcStmt
    srcSig <- f oldSig
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return oldSig

  machine
    :: forall s o
     . Width s
    => Width o
    => (Sig s -> Rtl (Sig s, Sig o))
    -> Rtl (Sig s, Sig o)
  machine f = do
    old <- freshWire $ width (undefined :: s)
    let oldSig = Sig old
    procStmt <- freshProcStmt
    (srcSig, outSig) <- f oldSig
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec (spec srcSig))]
    return (oldSig, outSig)

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
