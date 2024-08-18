{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Yosys [RTLIL](https://yosyshq.readthedocs.io/projects/yosys/en/latest/yosys_internals/formats/rtlil_text.html)
module Bayeux.Rtl
  ( binaryValue
  , top
  , initial
  , freshWireId
  , freshCellId
  , freshProcStmt
  , updateP
  , -- * Monad
    MonadRtl(..)
  , shl, shr, sshl, sshr
  , Err(..)
  , Rtl(..)
  , compile
  ) where

import Bayeux.Encode
import Bayeux.Width
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.String
import Data.Text (Text)
import Yosys.Rtl

binaryValue :: Encode b => Width b => b -> Value
binaryValue b = Value (width b) $ encode b

top :: [ModuleBody] -> File
top body = File (Just 0) [Module [] "\\top" body ModuleEndStmt]

initial
  :: Encode output
  => Width  output
  => Text -- ^ output identifier
  -> output
  -> [ModuleBody]
initial outputIdent output =
  [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 1] $ WireId $ Ident outputIdent
  , ModuleBodyConnStmt $ ConnStmt
      (SigSpecWireId $ WireId $ Ident outputIdent)
      (SigSpecConstant value)
  ]
  where
    value = let size = fromIntegral $ width output
                bs   = encode output
            in ConstantValue $ Value size bs

freshWireId :: Functor m => MonadRtl m => m WireId
freshWireId = ("\\wire" <>) . fromString . show <$> fresh

freshCellId :: Functor m => MonadRtl m => m CellId
freshCellId = ("$cell" <>) . fromString . show <$> fresh

freshProcStmt :: Functor m => MonadRtl m => m ProcStmt
freshProcStmt = ("$proc" <>) . fromString . show <$> fresh

updateP :: ProcStmt -> DestSigSpec -> SrcSigSpec -> Process
updateP procStmt destSig srcSig = Process
  []
  procStmt
  (ProcessBody
    []
    Nothing
    []
    [Sync
       (SyncStmt Posedge (SigSpecWireId "\\clk"))
       [UpdateStmt destSig srcSig]
    ]
  )
  ProcEndStmt

class MonadRtl m where
  fresh     :: m Integer
  freshWire :: Integer -- ^ width
            -> m SigSpec

  process :: Integer -- ^ width
          -> (SigSpec -> m SigSpec)
          -> m SigSpec
  -- | Output width=1
  at :: SigSpec -> Integer -> m SigSpec

  -- | If S == 1 then B else A
  mux :: Integer   -- ^ width
      -> SigSpec   -- ^ S
      -> SigSpec   -- ^ A
      -> SigSpec   -- ^ B
      -> m SigSpec -- ^ Y

  unary :: ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Bool
        -> Integer
        -> Integer
        -> SigSpec
        -> m SigSpec
  binary :: ( CellId
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
         -> Bool
         -> Integer
         -> Bool
         -> Integer
         -> Integer
         -> SigSpec
         -> SigSpec
         -> m SigSpec

  shift :: ( CellId
             -> Bool
             -> Integer
             -> Integer
             -> Integer
             -> SigSpec
             -> SigSpec
             -> SigSpec
             -> Cell
           )
        -> Bool
        -> Integer
        -> Integer
        -> Integer
        -> SigSpec
        -> SigSpec
        -> m SigSpec

data Err = SizeMismatch
         | SignedShift
  deriving (Eq, Read, Show)

newtype Rtl a = Rtl{ unRtl :: WriterT [ModuleBody] (ExceptT Err (State Integer)) a }
  deriving ( Functor, Applicative, Monad
           , MonadError Err
           , MonadWriter [ModuleBody]
           , MonadState Integer
           )

instance MonadRtl Rtl where
  fresh = state $ \i -> (i, i + 1)
  freshWire w = do
    wId <- freshWireId
    tell [ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth w] wId]
    return $ SigSpecWireId wId

  process w f = do
    old <- freshWire w
    procStmt <- freshProcStmt
    srcSig <- f old
    tell [ModuleBodyProcess $ updateP procStmt (DestSigSpec old) (SrcSigSpec srcSig)]
    return old

  at sigSpec ix = do
    y <- freshWire 1
    tell [ModuleBodyConnStmt $ ConnStmt y (SigSpecSlice sigSpec ix Nothing)]
    return y
  mux w s a b = do
    y <- freshWire w
    cId <- freshCellId
    tell [ModuleBodyCell $ muxC cId w a b s y]
    return y

  unary cFn aSigned aWidth yWidth a = do
    y <- freshWire yWidth
    cId <- freshCellId
    tell [ModuleBodyCell $ cFn cId aSigned aWidth yWidth a y]
    return y

  binary cFn aSigned aWidth bSigned bWidth yWidth a b = do
    y <- freshWire yWidth
    cId <- freshCellId
    tell [ModuleBodyCell $ cFn cId aSigned aWidth bSigned bWidth yWidth a b y]
    return y

  shift cFn aSigned aWidth bWidth yWidth a b = do
    y <- freshWire yWidth
    cId <- freshCellId
    tell [ModuleBodyCell $ cFn cId aSigned aWidth bWidth yWidth a b y]
    return y

shl, shr, sshl, sshr
  :: MonadRtl m
  => Bool
  -> Integer
  -> Integer
  -> Integer
  -> SigSpec
  -> SigSpec
  -> m SigSpec
shl = shift shlC
shr = shift shrC
sshl = shift sshlC
sshr = shift sshrC

compile :: Rtl a -> Either Err File
compile = fmap (top . clocked) . flip evalState 1 . runExceptT . execWriterT . unRtl

clocked :: [ModuleBody] -> [ModuleBody]
clocked = (ModuleBodyWire (Wire [] $ WireStmt [WireOptionInput 1] "\\clk") :)
