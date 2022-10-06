module CodeGen.ARM.RegAlloc where

import qualified Ir.Tac.Types as T

import qualified Data.Map.Strict as M

type Type = String

data Term 
    = Register Int 
    | StackLoc Int
    | ParamRegister Int 
    | Global String

type Inst = T.GeneralInst Term   

data Func = Func {getParamTypes :: [Type], getReturnType :: Type, getFunc :: [Inst]}

newtype Program = Program {getProgram :: M.Map String Func}
