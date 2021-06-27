module RegAlloc.Shared where 

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
