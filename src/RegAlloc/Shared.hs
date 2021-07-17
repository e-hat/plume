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

convertToPhysical :: VRegMapping -> [Inst] -> [Inst]
convertToPhysical rm = map updateRs
 where
  u :: Value -> Value
  u (VRegister v) =
    case rm M.! v of
      Register p -> PRegister $ toInteger p
      Spill l -> StackLoc l
  u v = v
  updateRs :: Inst -> Inst
  updateRs (Move v1 v2) = Move (u v1) (u v2)
  updateRs (Add v1 v2) = Add (u v1) (u v2)
  updateRs (Sub v1 v2) = Sub (u v1) (u v2)
  updateRs (Mul v1 v2) = Mul (u v1) (u v2)
  updateRs (Div v1 v2) = Div (u v1) (u v2)
  updateRs (Neg v) = Neg (u v)
  updateRs (IAnd v1 v2) = IAnd (u v1) (u v2)
  updateRs (IOr v1 v2) = IOr (u v1) (u v2)
  updateRs (Inv v) = Inv (u v)
  updateRs (Cmp v1 v2) = Cmp (u v1) (u v2)
  updateRs (Push v) = Push (u v)
  updateRs (Pop v) = Pop (u v)
  updateRs other = other
