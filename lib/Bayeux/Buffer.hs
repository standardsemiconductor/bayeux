{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Bayeux.Buffer
  ( MonadBuffer(..)
  ) where

import Bayeux.Cell
import qualified Bayeux.Cell as C
import Bayeux.Encode
import Bayeux.Rtl (Rtl)
import Bayeux.Signal
import Bayeux.Width
import Data.Finitary
import Data.Finite
import Data.String
import Data.Vector.Sized (Vector)
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeNats

class MonadBuffer m where
  buffer
    :: Finitary e
    => KnownNat n
    => Sig (Maybe e)
    -> m (Sig (Maybe (Vector n e)))

  cobuffer
    :: Finitary e
    => KnownNat n
    => Sig (Maybe (Vector n e))
    -> m (Sig (Maybe e))

instance MonadBuffer Rtl where
  buffer
    :: forall e n
     . Finitary e
    => KnownNat n
    => Sig (Maybe e)
    -> Rtl (Sig (Maybe (Vector n e)))
  buffer inp = do
    i <- process $ \i  -> do
      iPlusOne <- inc i
      i' <- patm i
        [ maxBound ~> val (0 :: Finite n)
        , wildm iPlusOne
        ]
      patm (sliceValid inp)
        [ True ~> i'
        , wildm i
        ]
    isFull <- i === val maxBound
    b <- process $ \b -> do
      let shamt :: Word8
          shamt = fromIntegral w
      shiftedBuf <- shr b $ val shamt
      let la = fromIntegral $ width (undefined :: Vector n e)
          le = fromIntegral w
          input' :: Sig (Vector n e)
          input' = Sig $ mconcat
            [ spec $ sliceValue inp
            , fromString $ show (la - le) <> "'" <> replicate (la - le) '0'
            ]
          mask :: Sig (Vector n e)
          mask  = let zs = replicate le '0'
                      ones = replicate (la - le) '1'
                  in fromString $ show la <> "'" <> zs <> ones
      maskedBuf <- shiftedBuf `C.and` mask
      buf' <- input' `C.or` maskedBuf
      patm (sliceValid inp)
        [ True ~> buf'
        , wildm b
        ]
    isValid' <- process $ const $ isFull `C.logicAnd` sliceValid inp
    return $ Sig $ spec isValid' <> spec b
    where
      w :: Integer
      w = width (undefined :: e)

  cobuffer
    :: forall e n
     . Finitary e
    => KnownNat n
    => Sig (Maybe (Vector n e))
    -> Rtl (Sig (Maybe e))
  cobuffer a = fmap snd $ machine $ \s -> do
    let fsmSig = sliceFsm s
        ixSig  = sliceIx  s
        bufSig = sliceBuf s :: Sig (Maybe (Vector n e))
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
    ix' <- flip (mux gotoBusy) (val 0) =<< patm ixSig
      [ maxBound ~> val (0 :: Finite n)
      , wildm ix1
      ]
    let shamt = fromIntegral (width (undefined :: e)) :: Word8
    bufValue' <- shr (sliceValue bufSig) $ val shamt
    buf' <- flip (mux gotoIdle) (val Nothing) =<< patm fsmSig
      [ Idle ~> a
      , wildm $ Sig $ "1'1" <> spec bufValue'
      ]
    let e  = slice (width (undefined :: e) - 1) 0 bufSig
        o  = Sig $ spec isBusy <> spec e
        s' = Sig $ spec fsm' <> spec ix' <> spec buf'
    return (s', o)
    where
      aValid = sliceValid a

data Fsm = Idle | Busy
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Finitary)

data Cobuf n e = Cobuf
  { fsm :: Fsm
  , ix  :: Finite n
  , buf :: Maybe (Vector n e)
  }
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Finitary)

sliceFsm :: KnownNat n => Finitary e => Sig (Cobuf n e) -> Sig Fsm
sliceFsm s = slice (width s - 1) (width s - 1) s

sliceIx :: forall n e. KnownNat n => Finitary e => Sig (Cobuf n e) -> Sig (Finite n)
sliceIx s = slice (width s - 2) (width (undefined :: Maybe (Vector n e))) s

sliceBuf
  :: forall n e
   . KnownNat n
  => Finitary e
  => Sig (Cobuf n e)
  -> Sig (Maybe (Vector n e))
sliceBuf = slice (width (undefined :: Maybe (Vector n e)) - 1) 0
