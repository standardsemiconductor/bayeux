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

newtype Ident = Ident Text
  deriving (Eq, Read, Show)

data Value = Value Integer [BinaryDigit]
  deriving (Eq, Read, Show)

data BinaryDigit = B0
                 | B1
                 | X
                 | Z
                 | M
                 | D
  deriving (Eq, Read, Show)

data File = File (Maybe AutoIdxStmt) [Module]
  deriving (Eq, Read, Show)

newtype AutoIdxStmt = AutoIdxStmt Integer
  deriving (Eq, Read, Show)

data Module = Module [AttrStmt] [ModuleBody] ModuleEndStmt
  deriving (Eq, Read, Show)

newtype ModuleStmt = ModuleStmt Ident
  deriving (Eq, Read, Show)

data ModuleBody = ModuleBodyParamStmt ParamStmt
                | ModuleBodyWire Wire
                | ModuleBodyMemory Memory
                | ModuleBodyCell Cell
                | ModuleBodyProcess Process
  deriving (Eq, Read, Show)

data ParamStmt = ParamStmt Ident (Maybe Constant)
  deriving (Eq, Read, Show)

data Constant = ConstantValue Integer BinaryDigit
              | ConstantInteger Integer
              | ConstantString Text
  deriving (Eq, Read, Show)

data ModuleEndStmt = ModuleEndStmt
  deriving (Eq, Read, Show)

data AttrStmt = AttrStmt Ident Constant
  deriving (Eq, Read, Show)

data SigSpec = SigSpecConstant Constant
             | SigSpecWireId
             | SigSpecSlice
             | SigSpecCat
  deriving (Eq, Read, Show)

data ConnStmt = ConnStmt SigSpec SigSpec
  deriving (Eq, Read, Show)

data Wire = Wire [AttrStmt] WireStmt
  deriving (Eq, Read, Show)

data WireStmt = WireStmt [WireOption] WireId
  deriving (Eq, Read, Show)

newtype WireId = WireId Ident
  deriving (Eq, Read, Show)

data WireOption = WireOptionWidth  Integer
                | WireOptionOffset Integer
                | WireOptionInput  Integer
                | WireOptionOutput Integer
                | WireOptionInout  Integer
                | WireOptionUpto
                | WireOptionSigned
  deriving (Eq, Read, Show)

data Memory = Memory [AttrStmt] MemoryStmt
  deriving (Eq, Read, Show)

data MemoryStmt = MemoryStmt [MemoryOption] Ident
  deriving (Eq, Read, Show)

data MemoryOption = MemoryOptionWidth  Integer
                  | MemoryOptionSize   Integer
                  | MemoryOptionOffset Integer
  deriving (Eq, Read, Show)

data Cell = Cell [AttrStmt] CellStmt [CellBodyStmt] CellEndStmt
  deriving (Eq, Read, Show)

data CellStmt = CellStmt CellType CellId
  deriving (Eq, Read, Show)

newtype CellId = CellId Ident
  deriving (Eq, Read, Show)

newtype CellType = CellType Ident
  deriving (Eq, Read, Show)

data CellBodyStmt = CellParameter Bool Ident Constant
                  | CellConnect Ident SigSpec
  deriving (Eq, Read, Show)

data CellEndStmt = CellEndStmt
  deriving (Eq, Read, Show)

data Process = Process [AttrStmt] ProcStmt ProcessBody ProcEndStmt
  deriving (Eq, Read, Show)

newtype ProcStmt = ProcStmt Ident
  deriving (Eq, Read, Show)

data ProcessBody = ProcessBody [AssignStmt] (Maybe Switch) [AssignStmt] [Sync]
  deriving (Eq, Read, Show)

data AssignStmt = AssignStmt DestSigSpec SrcSigSpec
  deriving (Eq, Read, Show)

newtype DestSigSpec = DestSigSpec SigSpec
  deriving (Eq, Read, Show)

newtype SrcSigSpec = SrcSigSpec SigSpec
  deriving (Eq, Read, Show)

data ProcEndStmt = ProcEndStmt
  deriving (Eq, Read, Show)

data Switch = Switch SwitchStmt [Case] SwitchEndStmt
  deriving (Eq, Read, Show)

data SwitchStmt = SwitchStmt [AttrStmt] SigSpec
  deriving (Eq, Read, Show)

data Case = Case [AttrStmt] CaseStmt CaseBody
  deriving (Eq, Read, Show)

newtype CaseStmt = CaseStmt (Maybe Compare)
  deriving (Eq, Read, Show)

data Compare = Compare SigSpec [SigSpec]
  deriving (Eq, Read, Show)

newtype CaseBody = CaseBody [Either Switch AssignStmt]
  deriving (Eq, Read, Show)

data SwitchEndStmt = SwitchEndStmt
  deriving (Eq, Read, Show)

data Sync = Sync SyncStmt [UpdateStmt]
  deriving (Eq, Read, Show)

data SyncStmt = SyncStmt SyncType SigSpec
              | SyncStmtGlobal
              | SyncStmtInit
              | SyncStmtAlways
  deriving (Eq, Read, Show)

data SyncType = Low
              | High
              | Posedge
              | Negedge
              | Edge
  deriving (Eq, Read, Show)

data UpdateStmt = UpdateStmt DestSigSpec SrcSigSpec
  deriving (Eq, Read, Show)
