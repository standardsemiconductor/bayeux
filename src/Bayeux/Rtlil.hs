{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Yosys [RTLIL](https://yosyshq.readthedocs.io/projects/yosys/en/latest/yosys_internals/formats/rtlil_text.html)
module Bayeux.Rtlil
  ( -- * Lexical elements
    Ident(..)
  , Value(..)
  , BinaryDigit(..)
  , -- * File
    File(..)
  , -- ** Autoindex statements
    AutoIdxStmt(..)
  , -- ** Modules
    Module(..)
  , ModuleStmt(..)
  , ModuleBody(..)
  , ParamStmt(..)
  , Constant(..)
  , ModuleEndStmt(..)
  , -- ** Attribute statements
    AttrStmt(..)
  , -- ** Signal specifications
    SigSpec(..)
  , -- ** Connections
    ConnStmt(..)
  , -- ** Wires
    Wire(..)
  , WireStmt(..)
  , WireId(..)
  , WireOption(..)
  , -- ** Memories
    Memory(..)
  , MemoryStmt(..)
  , MemoryOption(..)
  , -- ** Cells
    Cell(..)
  , CellStmt(..)
  , CellId(..)
  , CellType(..)
  , CellBodyStmt(..)
  , CellEndStmt(..)
  , sbRgbaDrv
  , -- ** Processes
    Process(..)
  , ProcStmt(..)
  , ProcessBody(..)
  , AssignStmt(..)
  , DestSigSpec(..)
  , SrcSigSpec(..)
  , ProcEndStmt(..)
  , -- ** Switches
    Switch(..)
  , SwitchStmt(..)
  , Case(..)
  , CaseStmt(..)
  , Compare(..)
  , CaseBody(..)
  , SwitchEndStmt(..)
  , -- ** Syncs
    Sync(..)
  , SyncStmt(..)
  , SyncType(..)
  , UpdateStmt(..)
  ) where

import Data.Text (Text)
import Prettyprinter

newtype Ident = Ident Text
  deriving (Eq, Pretty, Read, Show)

data Value = Value Integer [BinaryDigit]
  deriving (Eq, Read, Show)

instance Pretty Value where
  pretty (Value i bs) = pretty i <> "\'" <> foldMap pretty bs

data BinaryDigit = B0
                 | B1
                 | X
                 | Z
                 | M
                 | D
  deriving (Eq, Read, Show)

instance Pretty BinaryDigit where
  pretty = \case
    B0 -> "0"
    B1 -> "1"
    X  -> "x"
    Z  -> "z"
    M  -> "m"
    D  -> "-"

data File = File (Maybe AutoIdxStmt) [Module]
  deriving (Eq, Read, Show)

instance Pretty File where
  pretty (File iM ms) = let ms' = pretty <$> ms
                        in vsep $ case iM of
                             Just i  -> pretty i : ms'
                             Nothing -> ms'

newtype AutoIdxStmt = AutoIdxStmt Integer
  deriving (Eq, Num, Read, Show)

instance Pretty AutoIdxStmt where
  pretty (AutoIdxStmt i) = "autoidx" <+> pretty i

data Module = Module [AttrStmt] ModuleStmt [ModuleBody] ModuleEndStmt
  deriving (Eq, Read, Show)

instance Pretty Module where
  pretty (Module as m bs e) = vsep
    [ vsep $ pretty <$> as
    , pretty m
    , indent 2 $ vsep $ pretty <$> bs
    , pretty e
    ]

newtype ModuleStmt = ModuleStmt Ident
  deriving (Eq, Read, Show)

instance Pretty ModuleStmt where
  pretty (ModuleStmt i) = "module" <+> pretty i <> hardline

data ModuleBody = ModuleBodyParamStmt ParamStmt
                | ModuleBodyWire Wire
                | ModuleBodyMemory Memory
                | ModuleBodyCell Cell
                | ModuleBodyProcess Process
  deriving (Eq, Read, Show)

instance Pretty ModuleBody where
  pretty = \case
    ModuleBodyParamStmt p -> pretty p
    ModuleBodyWire      w -> pretty w
    ModuleBodyMemory    m -> pretty m
    ModuleBodyCell      c -> pretty c
    ModuleBodyProcess   p -> pretty p

data ParamStmt = ParamStmt Ident (Maybe Constant)
  deriving (Eq, Read, Show)

instance Pretty ParamStmt where
  pretty (ParamStmt i cM) = mconcat
    [ "parameter" <+> pretty i
    , maybe mempty (surround " " " " . pretty) cM
    , hardline
    ]

data Constant = ConstantValue Value
              | ConstantInteger Integer
              | ConstantString Text
  deriving (Eq, Read, Show)

instance Pretty Constant where
  pretty = \case
    ConstantValue   v -> pretty v
    ConstantInteger i -> pretty i
    ConstantString  t -> dquotes $ pretty t

data ModuleEndStmt = ModuleEndStmt
  deriving (Eq, Read, Show)

instance Pretty ModuleEndStmt where
  pretty _ = "end" <> hardline

data AttrStmt = AttrStmt Ident Constant
  deriving (Eq, Read, Show)

instance Pretty AttrStmt where
  pretty (AttrStmt i c) = "attribute" <+> pretty i <+> pretty c <> hardline

data SigSpec = SigSpecConstant Constant
             | SigSpecWireId   WireId
             | SigSpecSlice    SigSpec Integer (Maybe Integer)
             | SigSpecCat      [SigSpec]
  deriving (Eq, Read, Show)

instance Pretty SigSpec where
  pretty = \case
    SigSpecConstant c   -> pretty c
    SigSpecWireId w     -> pretty w
    SigSpecSlice s x yM -> pretty s <+> brackets (pretty x <> maybe mempty ((":" <>) . pretty) yM)
    SigSpecCat ss       -> braces $ foldMap pretty ss

data ConnStmt = ConnStmt SigSpec SigSpec
  deriving (Eq, Read, Show)

instance Pretty ConnStmt where
  pretty (ConnStmt x y) = "connect" <+> pretty x <+> pretty y <> hardline

data Wire = Wire [AttrStmt] WireStmt
  deriving (Eq, Read, Show)

instance Pretty Wire where
  pretty (Wire as s) = foldMap pretty as <> pretty s

data WireStmt = WireStmt [WireOption] WireId
  deriving (Eq, Read, Show)

instance Pretty WireStmt where
  pretty (WireStmt os i) = "wire" <+> hsep (pretty <$> os) <+> pretty i <> hardline

newtype WireId = WireId Ident
  deriving (Eq, Pretty, Read, Show)

data WireOption = WireOptionWidth  Integer
                | WireOptionOffset Integer
                | WireOptionInput  Integer
                | WireOptionOutput Integer
                | WireOptionInout  Integer
                | WireOptionUpto
                | WireOptionSigned
  deriving (Eq, Read, Show)

instance Pretty WireOption where
  pretty = \case
    WireOptionWidth  i -> "width"  <+> pretty i
    WireOptionOffset i -> "offset" <+> pretty i
    WireOptionInput  i -> "input"  <+> pretty i
    WireOptionOutput i -> "output" <+> pretty i
    WireOptionInout  i -> "inout"  <+> pretty i
    WireOptionUpto     -> "upto"
    WireOptionSigned   -> "signed"

data Memory = Memory [AttrStmt] MemoryStmt
  deriving (Eq, Read, Show)

instance Pretty Memory where
  pretty (Memory as s) = hsep $ pretty `fmap` as <> [pretty s]

data MemoryStmt = MemoryStmt [MemoryOption] Ident
  deriving (Eq, Read, Show)

instance Pretty MemoryStmt where
  pretty (MemoryStmt os i) = "memory" <> foldMap pretty os <> pretty i

data MemoryOption = MemoryOptionWidth  Integer
                  | MemoryOptionSize   Integer
                  | MemoryOptionOffset Integer
  deriving (Eq, Read, Show)

instance Pretty MemoryOption where
  pretty = \case
    MemoryOptionWidth  i -> "width"  <+> pretty i
    MemoryOptionSize   i -> "size"   <+> pretty i
    MemoryOptionOffset i -> "offset" <+> pretty i

data Cell = Cell [AttrStmt] CellStmt [CellBodyStmt] CellEndStmt
  deriving (Eq, Read, Show)

instance Pretty Cell where
  pretty (Cell as s bs e) = vsep
    [ foldMap pretty as <> pretty s
    , indent 2 $ foldMap pretty bs
    , pretty e
    ]

data CellStmt = CellStmt CellType CellId
  deriving (Eq, Read, Show)

instance Pretty CellStmt where
  pretty (CellStmt t i) = "cell" <+> pretty t <+> pretty i <> hardline

newtype CellId = CellId Ident
  deriving (Eq, Pretty, Read, Show)

newtype CellType = CellType Ident
  deriving (Eq, Pretty, Read, Show)

data ParamType = Signed | Real
  deriving (Eq, Read, Show)

instance Pretty ParamType where
  pretty Signed = "signed"
  pretty Real   = "real"

data CellBodyStmt = CellParameter (Maybe ParamType) Ident Constant
                  | CellConnect Ident SigSpec
  deriving (Eq, Read, Show)

instance Pretty CellBodyStmt where
  pretty = \case
    CellParameter Nothing i c -> "parameter" <+> pretty i <+> pretty c <> hardline
    CellParameter (Just p) i c -> "parameter" <+> pretty p <+> pretty i <+> pretty c <> hardline
    CellConnect i s -> "connect" <+> pretty i <+> pretty s <> hardline

data CellEndStmt = CellEndStmt
  deriving (Eq, Read, Show)

instance Pretty CellEndStmt where
  pretty _ = "end" <> hardline

sbRgbaDrv :: Cell
sbRgbaDrv = Cell
  [AttrStmt (Ident "\\module_not_derived") $ ConstantInteger 1]
  (CellStmt (CellType $ Ident "\\SB_RGBA_DRV") (CellId $ Ident "\\RGBA_DRIVER"))
  [ CellParameter Nothing (Ident "\\CURRENT_MODE") $ ConstantString "0b1"
  , CellParameter Nothing (Ident "\\RGB0_CURRENT") $ ConstantString "0b111111"
  , CellParameter Nothing (Ident "\\RGB1_CURRENT") $ ConstantString "0b111111"
  , CellParameter Nothing (Ident "\\RGB2_CURRENT") $ ConstantString "0b111111"
  , CellConnect (Ident "\\CURREN") $ SigSpecConstant $ ConstantValue $ Value 1 [B1]
  , CellConnect (Ident "\\RGB0") $ SigSpecWireId $ WireId $ Ident "\\red"
  , CellConnect (Ident "\\RGB0PWM") $ SigSpecWireId $ WireId $ Ident "\\pwm_r"
  , CellConnect (Ident "\\RGB1") $ SigSpecWireId $ WireId $ Ident "\\green"
  , CellConnect (Ident "\\RGB1PWM") $ SigSpecWireId $ WireId $ Ident "\\pwm_g"
  , CellConnect (Ident "\\RGB2") $ SigSpecWireId $ WireId $ Ident "\\blue"
  , CellConnect (Ident "\\RGB2PWM") $ SigSpecWireId $ WireId $ Ident "\\pwm_b"
  , CellConnect (Ident "\\RGBLEDEN") $ SigSpecConstant $ ConstantValue $ Value 1 [B1]
  ]
  CellEndStmt

data Process = Process [AttrStmt] ProcStmt ProcessBody ProcEndStmt
  deriving (Eq, Read, Show)

instance Pretty Process where
  pretty (Process as s b e) = vsep
    [ foldMap pretty as <> pretty s
    , indent 2 $ pretty b
    , pretty e
    ]

newtype ProcStmt = ProcStmt Ident
  deriving (Eq, Read, Show)

instance Pretty ProcStmt where
  pretty (ProcStmt i) = "process" <+> pretty i <> hardline

data ProcessBody = ProcessBody [AssignStmt] (Maybe Switch) [AssignStmt] [Sync]
  deriving (Eq, Read, Show)

instance Pretty ProcessBody where
  pretty (ProcessBody as sM bs ss) = vsep
    [ foldMap pretty as
    , maybe mempty pretty sM
    , foldMap pretty bs
    , foldMap pretty ss
    ]

data AssignStmt = AssignStmt DestSigSpec SrcSigSpec
  deriving (Eq, Read, Show)

instance Pretty AssignStmt where
  pretty (AssignStmt d s) = "assign" <+> pretty d <+> pretty s <> hardline

newtype DestSigSpec = DestSigSpec SigSpec
  deriving (Eq, Pretty, Read, Show)

newtype SrcSigSpec = SrcSigSpec SigSpec
  deriving (Eq, Pretty, Read, Show)

data ProcEndStmt = ProcEndStmt
  deriving (Eq, Read, Show)

instance Pretty ProcEndStmt where
  pretty _ = "end" <> hardline

data Switch = Switch SwitchStmt [Case] SwitchEndStmt
  deriving (Eq, Read, Show)

instance Pretty Switch where
  pretty (Switch s cs e) = pretty s <> foldMap pretty cs <> pretty e

data SwitchStmt = SwitchStmt [AttrStmt] SigSpec
  deriving (Eq, Read, Show)

instance Pretty SwitchStmt where
  pretty (SwitchStmt as s) = foldMap pretty as <> "switch" <+> pretty s <> hardline

data Case = Case [AttrStmt] CaseStmt CaseBody
  deriving (Eq, Read, Show)

instance Pretty Case where
  pretty (Case as s b) = foldMap pretty as <> pretty s <+> pretty b

newtype CaseStmt = CaseStmt (Maybe Compare)
  deriving (Eq, Read, Show)

instance Pretty CaseStmt where
  pretty (CaseStmt Nothing)  = "case" <> hardline
  pretty (CaseStmt (Just c)) = "case" <+> pretty c <> hardline

data Compare = Compare SigSpec [SigSpec]
  deriving (Eq, Read, Show)

instance Pretty Compare where
  pretty (Compare s ss) = hsep $ punctuate "," $ pretty <$> s : ss

newtype CaseBody = CaseBody [Either Switch AssignStmt]
  deriving (Eq, Read, Show)

instance Pretty CaseBody where
  pretty (CaseBody es) = vsep $ either pretty pretty <$> es

data SwitchEndStmt = SwitchEndStmt
  deriving (Eq, Read, Show)

instance Pretty SwitchEndStmt where
  pretty _ = "end" <> hardline

data Sync = Sync SyncStmt [UpdateStmt]
  deriving (Eq, Read, Show)

instance Pretty Sync where
  pretty (Sync s us) = vsep
    [ pretty s
    , indent 2 $ foldMap pretty us
    ]

data SyncStmt = SyncStmt SyncType SigSpec
              | SyncStmtGlobal
              | SyncStmtInit
              | SyncStmtAlways
  deriving (Eq, Read, Show)

instance Pretty SyncStmt where
  pretty = \case
    SyncStmt t s   -> "sync" <+> pretty t <+> pretty s <> hardline
    SyncStmtGlobal -> "sync" <+> "global" <> hardline
    SyncStmtInit   -> "sync" <+> "init"   <> hardline
    SyncStmtAlways -> "sync" <+> "always" <> hardline

data SyncType = Low
              | High
              | Posedge
              | Negedge
              | Edge
  deriving (Eq, Read, Show)

instance Pretty SyncType where
  pretty = \case
    Low     -> "low"
    High    -> "high"
    Posedge -> "posedge"
    Negedge -> "negedge"
    Edge    -> "edge"

data UpdateStmt = UpdateStmt DestSigSpec SrcSigSpec
  deriving (Eq, Read, Show)

instance Pretty UpdateStmt where
  pretty (UpdateStmt d s) = "update" <+> pretty d <+> pretty s <> hardline
