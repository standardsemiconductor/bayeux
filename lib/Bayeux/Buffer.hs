{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Bayeux.Buffer
  ( MonadBuffer(..)
  ) where

import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl (Rtl)
import Bayeux.Signal hiding (input)
import Bayeux.Width
import Data.Array
import Data.Finite
import Data.String
import Data.Word
import GHC.TypeNats

class MonadBuffer m where
  buffer
    :: Encode e
    => Width e
    => KnownNat n
    => Sig (Maybe e)
    -> m (Sig (Maybe (Array (Finite n) e)))

  cobuffer
    :: Encode e
    => Width e
    => KnownNat n
    => Sig (Maybe (Array (Finite n) e))
    -> m (Sig (Maybe e))

instance MonadBuffer Rtl where
  buffer
    :: forall e n
     . Encode e
    => Width e
    => KnownNat n
    => Sig (Maybe e)
    -> Rtl (Sig (Maybe (Array (Finite n) e)))
  buffer input = do
    ix <- process $ \ix  -> do
      ixPlusOne <- inc ix
      ix' <- patm ix
        [ maxBound ~> val (0 :: Finite n)
        , wildm ixPlusOne
        ]
      patm (sliceValid input)
        [ True ~> ix'
        , wildm ix
        ]
    isFull <- ix === val maxBound
    buf <- process $ \buf -> do
      let shamt :: Word8
          shamt = fromIntegral w
      shiftedBuf <- shr buf $ val shamt
      let la = fromIntegral $ width (undefined :: Array (Finite n) e)
          le = fromIntegral w
          input' :: Sig (Array (Finite n) e)
          input' = Sig $ mconcat
            [ spec (sliceValue input)
            , fromString $ show ((la - 1) * le) <> "'" <> concat (replicate (la - 1) (replicate le '0'))
            ]
          mask :: Sig (Array (Finite n) e)
          mask  = let zs = replicate le '0'
                      ones = replicate (la - le) '1'
                  in fromString $ show la <> "'" <> zs <> ones
      maskedBuf <- shiftedBuf `C.and` mask
      buf' <- input' `C.or` maskedBuf
      patm (sliceValid input)
        [ True ~> buf'
        , wildm buf
        ]
    isValid' <- process $ const $ isFull `C.logicAnd` sliceValid input
    return $ Sig $ spec isValid' <> spec buf
    where
      w :: Integer
      w = width (undefined :: e)

  cobuffer
    :: forall e n
     . Encode e
    => Width e
    => KnownNat n
    => Sig (Maybe (Array (Finite n) e))
    -> Rtl (Sig (Maybe e))
  cobuffer a = fmap snd $ machine $ \s -> do
    let fsmSig = sliceFsm s
        ixSig  = sliceIx  s
        bufSig = sliceBuf s
    isIdle <- fsmSig === val Idle
    isBusy <- fsmSig === val Busy
    ixMax <- ixSig === val maxBound
    gotoIdle <- isBusy `logicAnd` ixMax
    gotoBusy <- isIdle `logicAnd` aValid
    fsm' <- ifm
      [ gotoIdle `thenm` val Idle
      , gotoBusy `thenm` val Busy
      , elsem fsmSig
      ]
    ix1 <- inc ixSig
    ix' <- patm ixSig
      [ maxBound ~> val (0 :: Finite n)
      , wildm ix1
      ]
    ix'' <- mux gotoBusy ix' $ val 0
    let shamt = fromIntegral (width (undefined :: e)) :: Word8
    buf' <- shr bufSig $ val shamt
    buf'' <- flip (mux gotoIdle) (val Nothing) =<< patm fsmSig
      [ Idle ~> a
      , wildm buf'
      ]
    let e  = slice (width (undefined :: e) - 1) 0 bufSig
        o  = Sig $ (spec . sliceValid) bufSig <> spec e
        s' = Sig $ spec fsm' <> spec ix'' <> spec buf''
    return (s', o)
    where
      aValid = sliceValid a
      aValue = sliceValue a

data Fsm = Idle | Busy
  deriving (Eq, Read, Show)

instance Width Fsm where
  width _ = 1

instance Encode Fsm where
  encode Idle = [B0]
  encode Busy = [B1]

data Cobuf n e = Cobuf
  { fsm :: Fsm
  , ix  :: Finite n
  , buf :: Maybe (Array (Finite n) e)
  }
  deriving (Eq, Read, Show)

instance (KnownNat n, Width e) => Width (Cobuf n e) where
  width s = width (fsm s) + width (ix s) + width (buf s)

instance (KnownNat n, Encode e, Width e) => Encode (Cobuf n e) where
  encode s = encode (fsm s) <> encode (ix s) <> encode (buf s)

sliceFsm :: KnownNat n => Width e => Sig (Cobuf n e) -> Sig Fsm
sliceFsm s = slice (width s - 1) (width s - 1) s

sliceIx :: forall n e. KnownNat n => Width e => Sig (Cobuf n e) -> Sig (Finite n)
sliceIx s = slice (width s - 2) (width (undefined :: Maybe (Array (Finite n) e))) s

sliceBuf
  :: forall n e
   . KnownNat n
  => Width e
  => Sig (Cobuf n e)
  -> Sig (Maybe (Array (Finite n) e))
sliceBuf = slice (width (undefined :: Maybe (Array (Finite n) e)) - 1) 0
