module Wasm.Translation (toWasm) where

import qualified Ir.Tac.Types as T
import Wasm.Types

import Control.Monad.State
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

data TState = TState{ getOriginalFunc :: T.Func
                    , getCurrentInsts :: [Instruction]
                    , getLocalEnv :: M.Map Int VarU32
                    }

funcBody :: T.Func -> FuncBody
funcBody f = 
  let initial = TState f [] M.empty
      (ls, final) = runState 
        (do 
          result <- localEntries
          instructions
          return result)
        initial
   in FuncBody ls (getCurrentInsts final)

localEntries :: State TState (Array LocalEntry)
localEntries =
  let step :: M.Map T.Type (S.Set Int) -> T.Inst -> M.Map T.Type (S.Set Int)
      step groups (T.Assignment (T.Local n t) _) =
        M.insert t (S.insert n existing) groups
       where
        existing = M.findWithDefault S.empty t groups
      step groups _ = groups
      localEntry :: T.Type -> S.Set Int -> LocalEntry
      localEntry t s = LocalEntry (VarU32 $ S.size s) (valueType t)
      buildEnv :: M.Map Int VarU32 -> S.Set Int -> M.Map Int VarU32 
      buildEnv = foldl groupStep
        where 
          groupStep :: M.Map Int VarU32 -> Int -> M.Map Int VarU32
          groupStep env local = M.insert local (VarU32 $ M.size env) env
   in do 
     st <- get
     let is = T.getFunc $ getOriginalFunc st
     let varsByType = foldl step M.empty is
     let env = foldl buildEnv M.empty (map snd $ M.assocs varsByType) 
     put st{getLocalEnv = env}
     return $ Array $ map (uncurry localEntry) (M.assocs varsByType)

instructions :: State TState ()
instructions = gets (T.getFunc . getOriginalFunc) >>= mapM_ inst 

getLocalIdx :: T.Symbol -> State TState VarU32
getLocalIdx T.Global{} = undefined
getLocalIdx (T.Param n _) = return $ VarU32 n
getLocalIdx (T.Local n _) = do 
  env <- gets getLocalEnv
  return $ env M.! n

appendInst :: Instruction -> State TState ()
appendInst i = do 
  st <- get
  put $ st{getCurrentInsts = getCurrentInsts st ++ [i]}

inst :: T.Inst -> State TState ()
inst (T.Return Nothing) = appendInst $ ControlFlow End
inst (T.Return (Just (T.None (T.LitInt n)))) = do 
  appendInst $ BasicInst $ I32Const $ fromIntegral n
  appendInst $ ControlFlow End
inst _ = undefined

term :: T.Term -> State TState ()
term (T.LitInt n) = appendInst $ BasicInst $ I32Const $ fromIntegral n
term (T.LitFloat d) = appendInst $ BasicInst $ F64Const d
term (T.LitBool True) = appendInst $ BasicInst $ I32Const 1
term (T.LitBool False) = appendInst $ BasicInst $ I32Const 0
term (T.Subs sym) = do 
  idx <- getLocalIdx sym
  appendInst $ BasicInst $ GetLocal idx
term _ = undefined

expr :: T.Expr T.Term -> State TState ()
expr = undefined

signatur :: T.Func -> FuncSignature
signatur (T.Func ps r _) =
  FuncSignature (Array $ map valueType ps) (Array [valueType r])

valueType :: T.Type -> ValueType
valueType "Int" = I32
valueType "Bool" = B
valueType "Float" = F32
valueType t = error ("cannot create ValueType for " ++ t)
