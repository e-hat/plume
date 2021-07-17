module X8664.StackExpansion (expandStack) where

import Bytecode.Types
import RegAlloc.Shared

import qualified Data.Map.Strict as M

-- This is a temporary necessity for compilation.
-- After RegAlloc runs, the spills are not actually mapped to any real stack
-- locations. They are simply "stack loc 1" and "stack loc 2".
-- Stack expansion maps these "virtual" stack locations to the actual stack locations,
-- which means the type of data in each stack location must be known. It currently
-- is not, so everything is convservatively assumed to be the largest data type,
-- which I'm saying is 8 bytes (no arrays yet). Later, I'll keep some type info
-- in my bytecode which will be hugely useful, thanks to Tekknolagi for that tip.

expandStack :: BytecodeProgram -> BytecodeProgram
expandStack b@(BytecodeProgram _ ltbl) =
  BytecodeProgram (rebuild ltbl $ M.map expandStackF (funcs b)) ltbl

expandStackF :: Func -> Func
expandStackF = map (traverseVals f)
  where 
    f :: Value -> Value
    f (StackLoc i) = StackLoc $ i * 8
    f v = v
