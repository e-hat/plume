module Wasm.Types where

import Data.Word
import Data.Int
import qualified Data.ByteString.Char8 as C

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

data Basic 
  = Nop
  | I32Const Int32

data Cf 
  = End

data BasicInst

data ValueType = B | I32 | I64 | F32 | F64 | Func

data ExternalKind = Function | Table | Memory | Global

data Export = Export 
  { getName :: Identifier 
  , getKind :: ExternalKind 
  , getIndex :: VarU32
  }

newtype Array a = Array{getArray :: [a]}

type Index = VarU32
newtype VarU32 = VarU32{getVarU32 :: Int}
type Byte = Word8
type Identifier = Array Byte

toByteArray :: String -> Array Byte
toByteArray = Array . map (toEnum . fromEnum)
