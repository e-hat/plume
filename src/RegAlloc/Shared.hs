module RegAlloc.Shared where

import Bytecode.Types

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data VRegAssignment = Register Int | Spill Int
type VRegMapping = M.Map Integer VRegAssignment

numPhysical :: Int
numPhysical = 10

regalloc :: (Func -> [Int] -> VRegMapping) -> BytecodeProgram -> BytecodeProgram
regalloc regMappings p@(BytecodeProgram _ ltbl) =
  let frms = M.intersectionWith ($) (M.map regMappings fs) regPools
      fs = funcs p
      regPools = M.map regPool fs
      regPool :: Func -> [Int]
      regPool is =
        S.toList $
          S.fromAscList [1 .. numPhysical] S.\\ usedPhysicals is
   in p{getInstructions = rebuild ltbl $ M.intersectionWith convertToPhysical frms fs}

usedPhysicals :: Func -> S.Set Int
usedPhysicals = foldl handleInst (S.fromList [])
 where
  handleInst :: S.Set Int -> Inst -> S.Set Int
  handleInst accum (Move v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Add v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Sub v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Mul v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Div v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Neg v) = singleVal accum v
  handleInst accum (IAnd v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (IOr v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Inv v) = singleVal accum v
  handleInst accum (Cmp v1 v2) = doubleVal accum [v1, v2]
  handleInst accum (Push v) = singleVal accum v
  handleInst accum (Pop v) = singleVal accum v
  handleInst accum _ = accum
  handleValue :: S.Set Int -> Value -> S.Set Int
  handleValue accum (PRegister r) = S.insert (fromInteger r) accum
  handleValue accum _ = accum
  doubleVal :: S.Set Int -> [Value] -> S.Set Int
  doubleVal = foldl handleValue
  singleVal :: S.Set Int -> Value -> S.Set Int
  singleVal = handleValue

traverseVals :: (Value -> Value) -> Inst -> Inst
traverseVals f (Move v1 v2) = Move (f v1) (f v2)
traverseVals f (Add v1 v2) = Add (f v1) (f v2)
traverseVals f (Sub v1 v2) = Sub (f v1) (f v2)
traverseVals f (Mul v1 v2) = Mul (f v1) (f v2)
traverseVals f (Div v1 v2) = Div (f v1) (f v2)
traverseVals f (Neg v) = Neg (f v)
traverseVals f (IAnd v1 v2) = IAnd (f v1) (f v2)
traverseVals f (IOr v1 v2) = IOr (f v1) (f v2)
traverseVals f (Inv v) = Inv (f v)
traverseVals f (Cmp v1 v2) = Cmp (f v1) (f v2)
traverseVals f (Push v) = Push (f v)
traverseVals f (Pop v) = Pop (f v)
traverseVals _ other = other

convertToPhysical :: VRegMapping -> Func -> Func
convertToPhysical rm = map (traverseVals u)
 where
  u :: Value -> Value
  u (VRegister v) =
    case rm M.! v of
      Register p -> PRegister $ toInteger p
      Spill l -> StackLoc l
  u v = v
