module CodeGen.ARM.RegAlloc where

import Data.Binary
import Data.Int
import Data.List
import qualified Data.Map.Strict as M
import qualified Ir.Tac.Types as T
import Text.Printf

type Type = String

data Memory = Register Int | Stack Int

data Term
  = MemoryLoc Type Memory
  | Immediate Type Int32

type Inst = T.GeneralInst Term

data Func = Func {getParamTypes :: [Type], getReturnType :: Type, getFunc :: [Inst], getStackSize :: Int}

newtype Program = Program {getProgram :: M.Map String Func}

instance Show Memory where
  show (Register n) = "r" ++ show n
  show (Stack n) = printf "[fp, #-%s]" (show n)

instance Show Term where
  show (MemoryLoc _ m) = show m
  show (Immediate _ n) = printf "0x%08x" n

instance Show Func where
  show = intercalate "\n" . concatMap T.showInst . getFunc

instance Show Program where
  show = intercalate "\n\n" . map step . M.assocs . getProgram
   where
    step :: (String, Func) -> String
    step (fname, f) = printf "define %s\n%s" fname (show f)

-- Spills every single variable
naiveRegAlloc :: T.Program -> Program
naiveRegAlloc = Program . M.map naiveRegAllocFunc . T.getProgram

naiveRegAllocFunc :: T.Func -> Func
naiveRegAllocFunc (T.Func paramTypes returnType insts) =
  let paramToStackLoc = M.fromList $ map (\i -> ("p" ++ show i, 16 - 4 * i)) (take (length paramTypes) [0 ..] :: [Int])
      (highestLoc, localToStackLoc) = foldl (foldl step) (20, M.empty) insts
      step :: (Int, M.Map String Int) -> T.Term -> (Int, M.Map String Int)
      step (nextStackLoc, m) (T.Subs (T.Local localId _)) =
        if M.member ("l" ++ show localId) m
          then (nextStackLoc, m)
          else (nextStackLoc + 4, M.insert ("l" ++ show localId) nextStackLoc m)
      step m _ = m
      stackLocs = M.union paramToStackLoc localToStackLoc
   in Func paramTypes returnType (map (fmap (replaceWithStackRead stackLocs)) insts) highestLoc

replaceWithStackRead :: M.Map String Int -> T.Term -> Term
replaceWithStackRead m (T.Subs (T.Param paramId typ)) = MemoryLoc typ $ Stack $ m M.! ("p" ++ show paramId)
replaceWithStackRead m (T.Subs (T.Local localId typ)) = MemoryLoc typ $ Stack $ m M.! ("l" ++ show localId)
replaceWithStackRead _ (T.Subs (T.Global globalId typ)) = undefined
replaceWithStackRead _ (T.LitInt n) = Immediate "Int" (fromInteger n)
replaceWithStackRead _ (T.LitFloat n) = Immediate "Float" ((decode $ encode n) :: Int32)
replaceWithStackRead _ (T.LitString{}) = Immediate "String" 0x0
replaceWithStackRead _ (T.LitBool b) = Immediate "Bool" $ if b then 0xfffffff else 0x0
replaceWithStackRead _ (T.LitChar c) = Immediate "Char" ((decode $ encode c) :: Int32)
