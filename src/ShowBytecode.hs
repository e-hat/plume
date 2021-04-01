module ShowBytecode where

import BytecodeGen

instance Show BytecodeProgram where
  show b = showBPAt b 0

showBPAt :: BytecodeProgram -> Integer -> String 
showBPAt (BytecodeProgram (i:is) tbl) loc = undefined
