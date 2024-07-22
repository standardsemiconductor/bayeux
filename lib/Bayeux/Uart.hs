{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bayeux.Uart
  ( MonadUart(..)
  , hello
  , echo
  , echoLine
  ) where

import Bayeux.Buffer
import Bayeux.Cell
import Bayeux.Encode
import Bayeux.Ice40.Spram
import Bayeux.Rtl hiding (at, binary, mux, process, shift, shr, unary)
import Bayeux.Signal
import Bayeux.Width
import Control.Monad
import Control.Monad.Writer
import Data.Array
import Data.Bits hiding (shift)
import Data.Char
import Data.Finite
import Data.Proxy
import Data.Word

class MonadUart m where
  transmit :: Word16                -- ^ baud
           -> Sig (Maybe Word8)
           -> m (Sig Bool)          -- ^ busy=True, idle=False

  receive  :: Word16                -- ^ baud
           -> Sig Bool              -- ^ rx
           -> m (Sig (Maybe Word8))

data Fsm = Idle | Start | Recv | Stop
  deriving (Enum, Eq, Read, Show)

instance Width Fsm where
  width _ = 2

instance Encode Fsm where
  encode = encode . finiteProxy (Proxy :: Proxy 4) . fromIntegral . fromEnum

instance MonadUart Rtl where
  transmit baud byte = process $ \txFsm -> do
    isStart <- txFsm === sig False
    txCtr <- process $ \txCtr -> ifm
      [ (pure isStart .|| txCtr === sig baud) `thenm` val 0
      , elsem $ inc txCtr
      ]
    ctrDone <- txCtr === sig baud
    notDone <- logicNot ctrDone
    txIx <- process $ \(txIx :: Sig Word8) -> ifm
      [ pure notDone     `thenm` pure txIx
      , (txIx === sig 9) `thenm` val 0
      , elsem $ inc txIx
      ]
    isStartFrame <- txIx === sig 0
    isEndFrame   <- txIx === sig 9
    buf <- process $ \buf -> do
      buf' <- buf `shr` sig True
      ifs [ isStart      `thens` sliceValue byte
          , isStartFrame `thens` buf
          , notDone      `thens` buf
          , elses buf'
          ]
    e <- buf `at` 0
    output "\\tx" =<< ifs
      [ isStart      `thens` sig True
      , isStartFrame `thens` sig False
      , isEndFrame   `thens` sig True
      , elses e
      ]
    txFsm' <- logicNot =<< ctrDone `logicAnd` isEndFrame
    mux isStart txFsm' $ sliceValid byte

  receive baud rx = do
    rxLow  <- rx === sig False
    rxHigh <- logicNot rxLow
    fmap snd $ machine $ \(s :: Sig (Word16, Fsm)) -> do
      let rxCtr = sliceFst s
          rxFsm = sliceSnd s
      isIdle  <- rxFsm === sig Idle
      isStart <- rxFsm === sig Start
      isRecv  <- rxFsm === sig Recv
      isBaudHalf        <- rxCtr === sig (baud `shiftR` 1)
      isBaudHalfRxStart <- isBaudHalf `logicAnd` isStart
      isBaud            <- rxCtr === sig baud
      isBaudRxRecv      <- isBaud `logicAnd` isRecv
      buf <- buffer $ toMaybeSig isBaudRxRecv rx
      gotoRxStart <- rxLow `logicAnd` isIdle
      gotoRxRecv  <- rxLow `logicAnd` isBaudHalfRxStart
      gotoRxStop  <- sliceValid buf `logicAnd` isRecv
      gotoRxIdle  <- (rxHigh `logicAnd` isBaudHalfRxStart)
                       .|| (pure isBaud .&& rxFsm === sig Stop)
      rxFsm' <- ifs
        [ gotoRxStart `thens` sig Start
        , gotoRxRecv  `thens` sig Recv
        , gotoRxStop  `thens` sig Stop
        , gotoRxIdle  `thens` sig Idle
        , elses rxFsm
        ]
      rxCtr' <- ifm
        [ ((isIdle `logicOr` isBaudHalfRxStart) .|| pure isBaud) `thenm` val 0
        , elsem $ inc rxCtr
        ]
      return (Sig{ spec = spec rxCtr' <> spec rxFsm' }, packMaybe buf)
    where
      packMaybe :: Sig (Maybe (Array (Finite 8) Bool)) -> Sig (Maybe Word8)
      packMaybe = Sig . spec

hello :: Monad m => MonadUart m => MonadSignal m => m (Sig Word32)
hello = process $ \timer -> do
  is5Sec <- timer === sig 60000000
  _ <- transmit 624 $ toMaybeSig is5Sec $ sig 0x61
  flip (mux is5Sec) (sig 0) =<< inc timer

echo :: Monad m => MonadUart m => MonadSignal m => m (Sig Bool)
echo = transmit 624 =<< receive 624 =<< input "\\rx"

data ELFsm = Buffering | Cobuffering
  deriving (Eq, Read, Show)

instance Width ELFsm where
  width _ = 1

instance Encode ELFsm where
  encode Buffering   = [B0]
  encode Cobuffering = [B1]

data EchoLine = EchoLine
  { rwAddr :: Word14
  , elFsm  :: ELFsm
  }
  deriving (Eq, Read, Show)

instance Width EchoLine where
  width _ = 15

instance Encode EchoLine where
  encode el = encode (rwAddr el) <> encode (elFsm el)

sliceRWAddr :: Sig EchoLine -> Sig Word14
sliceRWAddr = slice 14 1

sliceELFsm :: Sig EchoLine -> Sig ELFsm
sliceELFsm = slice 0 0

echoLine
  :: Monad       m
  => MonadBuffer m
  => MonadRtl    m
  => MonadSignal m
  => MonadSpram  m
  => MonadUart   m
  => MonadWriter [ModuleBody] m
  => m (Sig EchoLine)
echoLine = do
  wM <- receive 624 =<< input "\\rx"
  process $ \s -> do
    let rwAddrSig = sliceRWAddr s
        fsm = sliceELFsm s
    txBusy <- (\txBusy -> do
      txIdle <- logicNot txBusy
      pats fsm
        [ Buffering ~~> toMaybeSig
          (sliceValid wM)
          (wSig
            rwAddrSig
            (Sig $ "8'00000000" <> (spec . sliceValue) wM)
            (Sig "4'0011")
          )
        , Cobuffering ~~> toMaybeSig txIdle (rSig rwAddrSig)
        ]) >-< (transmit 624 . repack <=< memory)
    txIdle <- logicNot txBusy
    rwAddrSig' <- patm fsm
      [ Buffering ~> ifm
        [ (pure . sliceValid) wM `thenm` inc rwAddrSig
        , elsem $ pure rwAddrSig
        ]
      , Cobuffering ~> ifm
        [ pure txIdle `thenm` dec rwAddrSig
        , elsem $ pure rwAddrSig
        ]
      ]
    fsm' <- patm fsm
      [ Buffering ~> patm wM
        [ (Just . fromIntegral . ord) '\n' ~> val Cobuffering
        , wildm $ pure fsm
        ]
      , Cobuffering ~> ifm
        [ ((rwAddrSig === (slice 13 0 . sig) (0 :: Word16)) .&& pure txIdle) `thenm` val Buffering
        , elsem $ pure fsm
        ]
      ]
    return $ Sig $ spec rwAddrSig' <> spec fsm'
  where
    repack :: Sig (Maybe Word16) -> Sig (Maybe Word8)
    repack s = Sig $ (spec . sliceValid) s <> (spec . slice 7 0 . sliceValue) s

-- | Interconnect. Create a `Sig a`. Apply it to the first argument. Apply the result
-- to the second argument. Connect the result to the `Sig a`.
(>-<)
  :: forall m a b
   . Monad       m
  => MonadRtl    m
  => MonadSignal m
  => MonadWriter [ModuleBody] m
  => Width a
  => (Sig a -> m (Sig b))
  -> (Sig b -> m (Sig a))
  -> m (Sig a)
f >-< g = do
  a  <- freshWire (width (undefined :: a))
  a' <- g =<< f (Sig a)
  tell [ModuleBodyConnStmt $ ConnStmt a (spec a')]
  return $ Sig a
