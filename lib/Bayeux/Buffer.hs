{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Bayeux.Buffer
  ( MonadBuffer(..)
  , Cobuf(..)
  , Fsm(..)
  ) where

import Bayeux.Cell hiding (le)
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl (Rtl)
import Bayeux.Signal
import Bayeux.Width
import Data.Array
import Data.Finite
import Data.Proxy
import Data.String
import GHC.TypeNats
import Yosys.Rtl

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
  buffer inp = do
    i <- process $ \(i :: Sig (Finite n)) -> ifm
      [ (pure . sliceValid) inp `thenm` patm i
        [ maxBound ~> val 0
        , wildm $ inc i
        ]
      , elsem $ pure i
      ]
    isFull <- i === sig maxBound
    b <- process $ \(b :: Sig (Array (Finite n) e)) -> do
      let bLength :: Integer
          bLength = fromIntegral $ natVal (Proxy :: Proxy n)
          ixs :: [Finite n]
          ixs = finite <$> reverse [1..bLength - 1]
          b' :: Sig (Array (Finite n) e)
          b' = Sig $ mconcat
            [ spec $ sliceValue inp
            , foldMap (spec . flip sliceIx b) ixs
            ]
{-
      let shiftedBuf = sliceRotate 1 b
          la = fromIntegral $ width (undefined :: Array (Finite n) e)
          le = fromIntegral w
          input' :: Sig (Array (Finite n) e)
          input' = Sig $ mconcat
            [ spec $ sliceValue inp
            , fromString $ show (la - le) <> "'" <> replicate (la - le) '0'
            ]
          mask :: Sig (Array (Finite n) e)
          mask  = let zs = replicate le '0'
                      ones = replicate (la - le) '1'
                  in fromString $ show la <> "'" <> zs <> ones
      maskedBuf <- shiftedBuf `C.and` mask
      buf' <- input' `C.or` maskedBuf
-}
      ifs [ sliceValid inp `thens` b'
          , elses b
          ]
    isValid' <- process $ const $ isFull `logicAnd` sliceValid inp
    return $ toMaybeSig isValid' b
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
        ixSig  = sliceCobufIx  s
        bufSig = sliceBuf s :: Sig (Maybe (Array (Finite n) e))
    isIdle <- fsmSig === sig Idle
    isBusy <- fsmSig === sig Busy
    ixMax <- ixSig === sig maxBound
    gotoIdle <- isBusy `logicAnd` ixMax
    gotoBusy <- isIdle `logicAnd` sliceValid a
    fsm' <- ifs
      [ gotoIdle `thens` sig Idle
      , gotoBusy `thens` sig Busy
      , elses fsmSig
      ]
    ix' <- ifm
      [ (pure gotoBusy .|| ixSig === sig maxBound) `thenm` val (0 :: Finite n)
      , elsem $ inc ixSig
      ]
    buf' <- flip (mux gotoIdle) (sig Nothing) =<< pats fsmSig
      [ Idle ~~> a
      , wilds $ justSig $ sliceRotate 1 $ sliceValue bufSig
      ]
    let e  = slice (width (undefined :: e) - 1) 0 bufSig
        o  = Sig $ spec isBusy <> spec e
        s' = Sig $ spec fsm' <> spec ix' <> spec buf'
    return (s', o)

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
  width _ = width (undefined :: Fsm) + width (undefined :: Finite n) + width (undefined :: Maybe (Array (Finite n) e))

instance (KnownNat n, Encode e, Width e) => Encode (Cobuf n e) where
  encode s = encode (fsm s) <> encode (ix s) <> encode (buf s)

sliceFsm :: KnownNat n => Width e => Sig (Cobuf n e) -> Sig Fsm
sliceFsm s = slice (width s - 1) (width s - 1) s

sliceCobufIx :: forall n e. KnownNat n => Width e => Sig (Cobuf n e) -> Sig (Finite n)
sliceCobufIx s = slice (width s - 2) (width (undefined :: Maybe (Array (Finite n) e))) s

sliceBuf
  :: forall n e
   . KnownNat n
  => Width e
  => Sig (Cobuf n e)
  -> Sig (Maybe (Array (Finite n) e))
sliceBuf = slice (width (undefined :: Maybe (Array (Finite n) e)) - 1) 0
