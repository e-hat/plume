module Wasm.Translation (toWasm) where

import Wasm.Types
import qualified Ir.Tac.Types as T

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

toWasm :: T.Program -> Program
toWasm = Program . pure . genModule . T.getProgram

genModule :: M.Map String T.Func -> Module
genModule funcMapping = 
  let funcAssocs = M.assocs funcMapping
      start = startSection $ map fst funcAssocs
      types = typeSection $ map snd funcAssocs
      funcs = funcSection $ map fst funcAssocs
   in Module [start, types, funcs]

typeSection :: [T.Func] -> KnownSection
typeSection = Types . Array. map signatur

startSection :: [String] -> KnownSection
startSection = Start . VarU32 . fromIntegral . fromJust . elemIndex "main"

funcSection :: [String] -> KnownSection
funcSection fs = Funcs $ Array $ map VarU32 $ take (length fs) [0..]

signatur :: T.Func -> FuncSignature
signatur (T.Func ps r _) = 
  FuncSignature (Array $ map valueType ps) (Array [valueType r])

valueType :: T.Type -> ValueType
valueType "Int" = I32
valueType "Bool" = B
valueType "Float" = F32
valueType t = error ("cannot create ValueType for " ++ t)
