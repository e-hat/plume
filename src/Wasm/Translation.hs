module Wasm.Translation (toWasm) where

import qualified Ir.Tac.Types as T
import Wasm.Types

import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

toWasm :: T.Program -> Program
toWasm = Program . pure . genModule . T.getProgram

genModule :: M.Map String T.Func -> Module
genModule funcMapping =
  let funcAssocs = M.assocs funcMapping
      exports = exportsSection $ map fst funcAssocs
      types = typeSection $ map snd funcAssocs
      funcs = funcSection $ map fst funcAssocs
      code = codeSection $ map snd funcAssocs
   in Module [types, funcs, exports, code]

exportsSection :: [String] -> KnownSection
exportsSection names = Exports $ Array [Export (toByteArray "main") Function $ VarU32 $ fromIntegral $ fromJust $ elemIndex "main" names]

typeSection :: [T.Func] -> KnownSection
typeSection = Types . Array . map signatur

startSection :: [String] -> KnownSection
startSection = Start . VarU32 . fromIntegral . fromJust . elemIndex "main"

funcSection :: [String] -> KnownSection
funcSection fs = Funcs $ Array $ map VarU32 $ take (length fs) [0 ..]

codeSection :: [T.Func] -> KnownSection
codeSection fs = Code $ Array $ map funcBody fs

funcBody :: T.Func -> FuncBody
funcBody f = FuncBody (localEntries f) (instructions f)

localEntries :: T.Func -> Array LocalEntry
localEntries (T.Func _ _ is) =
  let step :: M.Map T.Type (S.Set Int) -> T.Inst -> M.Map T.Type (S.Set Int)
      step groups (T.Assignment (T.Local n t) _) =
        M.insert t (S.insert n existing) groups
       where
        existing = M.findWithDefault S.empty t groups
      step groups _ = groups
      varsByType = foldl step M.empty is
      localEntry :: T.Type -> S.Set Int -> LocalEntry
      localEntry t s = LocalEntry (VarU32 $ S.size s) (valueType t)
   in Array $ map (uncurry localEntry) (M.assocs varsByType)

instructions :: T.Func -> [Instruction]
instructions = concatMap inst . T.getFunc

inst :: T.Inst -> [Instruction]
inst (T.Return Nothing) = [ControlFlow End]
inst (T.Return (Just (T.None (T.LitInt n)))) = [BasicInst $ I32Const $ fromIntegral n, ControlFlow End]
inst _ = undefined

signatur :: T.Func -> FuncSignature
signatur (T.Func ps r _) =
  FuncSignature (Array $ map valueType ps) (Array [valueType r])

valueType :: T.Type -> ValueType
valueType "Int" = I32
valueType "Bool" = B
valueType "Float" = F32
valueType t = error ("cannot create ValueType for " ++ t)
