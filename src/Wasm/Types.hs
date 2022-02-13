module Wasm.Types where

import Data.Word

newtype Program = Program {getModules :: [Module]}

newtype Module = Module {getKnownSections :: [KnownSection]}

data KnownSection
  = Start VarU32
  | Types (Array FuncSignature)
  | Funcs (Array Index)
  | Code (Array FuncBody)

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

data Basic 
  = Nop

data Cf 
  = End

data BasicInst

data ValueType = B | I32 | I64 | F32 | F64 | Func

newtype Array a = Array{getArray :: [a]}

type Index = VarU32
newtype VarU32 = VarU32{getVarU32 :: Int}
type Byte = Word8
