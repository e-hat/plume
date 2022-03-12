module Wasm.Types where

import Data.Int
import Data.Word

newtype Program = Program {getModules :: [Module]}

newtype Module = Module {getKnownSections :: [KnownSection]}

data KnownSection
  = Start VarU32
  | Types (Array FuncSignature)
  | Funcs (Array Index)
  | Code (Array FuncBody)
  | Exports (Array Export)

data FuncSignature = FuncSignature
  { getParams :: Array ValueType
  , getReturns :: Array ValueType
  }

data FuncBody = FuncBody
  { getLocals :: Array LocalEntry
  , getInstructions :: [Instruction]
  }

data LocalEntry = LocalEntry
  { getCount :: VarU32
  , getType :: ValueType
  }

data Instruction
  = BasicInst Basic
  | ControlFlow Cf
  | IntArithInst IntArith
  | FloatArithInst FloatArith
  | IntCmpInst IntCmp
  | FloatCmpInst FloatCmp

data Basic
  = Nop
  | I32Const Int32
  | F64Const Double
  | GetLocal VarU32
  | SetLocal VarU32

data Cf
  = End

data IntArith 
  = I32Sub 
  | I32Eqz
  | I32Add
  | I32Mul
  | I32DivS
  | I32And
  | I32Or

data FloatArith 
  = F64Sub
  | F64Add
  | F64Mul
  | F64Div

data IntCmp 
  = I32LtS
  | I32LeS
  | I32GtS
  | I32GeS
  | I32Eq 
  | I32Ne

data FloatCmp 
  = F64Eq
  | F64Ne 
  | F64Lt
  | F64Le
  | F64Gt
  | F64Ge

data ValueType = B | I32 | I64 | F32 | F64 | Func

data ExternalKind = Function | Table | Memory | Global

data Export = Export
  { getName :: Identifier
  , getKind :: ExternalKind
  , getIndex :: VarU32
  }

newtype Array a = Array {getArray :: [a]}

type Index = VarU32
newtype VarU32 = VarU32 {getVarU32 :: Int}
type Byte = Word8
type Identifier = Array Byte

toByteArray :: String -> Array Byte
toByteArray = Array . map (toEnum . fromEnum)
