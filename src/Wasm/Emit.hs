module Wasm.Emit (emit) where

import Wasm.Types

import Data.Binary.Put
import qualified Data.Binary.SLEB128 as SLEB
import qualified Data.Binary.ULEB128 as ULEB
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List
import Data.Word

class Emit a where
  emit :: a -> Put

instance Emit Program where
  emit (Program ms) = do
    header
    emit $ head ms

instance Emit Module where
  emit (Module sections) = do
    let sortedSections = sortBy (\l r -> opcode l `compare` opcode r) sections
    mapM_ emit sortedSections

instance Emit KnownSection where
  emit k = do
    putOpcode $ opcode k
    putAsByteArray $ payload k

instance Emit a => Emit (Array a) where
  emit (Array as) = do
    putVarUInt32 $ fromIntegral $ length as
    mapM_ emit as

instance Emit VarU32 where
  emit (VarU32 n) = ULEB.putWord $ fromIntegral n

instance Emit Word8 where
  emit = putWord8

instance Emit FuncSignature where
  emit (FuncSignature ps rs) = do
    emit Func
    emit ps
    emit rs

instance Emit FuncBody where
  emit (FuncBody ls is) = putAsByteArray $ emit ls >> mapM_ emit is

instance Emit LocalEntry where
  emit (LocalEntry n t) = do
    emit n
    emit t

instance Emit Instruction where
  emit (BasicInst b) = emit b
  emit (ControlFlow c) = emit c
  emit (IntArithInst i) = emit i
  emit (FloatArithInst f) = emit f
  emit (IntCmpInst i) = emit i
  emit (FloatCmpInst f) = emit f

instance Emit Basic where
  emit Nop = do
    putWord8 0x1
  emit (I32Const n) = do
    putWord8 0x41
    putVarSInt32 n
  emit (F64Const d) = do
    putWord8 0x44
    putF64 d
  emit (GetLocal i) = do
    putWord8 0x20
    putVarUInt32 $ fromIntegral $ getVarU32 i
  emit (SetLocal i) = do
    putWord8 0x21
    putVarUInt32 $ fromIntegral $ getVarU32 i

instance Emit Cf where
  emit End = do
    putWord8 0xb
  emit (Block sig) = do
    putWord8 0x2
    emit sig


instance Emit IntArith where
  emit I32Sub = putWord8 0x6b
  emit I32Eqz = putWord8 0x45
  emit I32Add = putWord8 0x6a
  emit I32Mul = putWord8 0x6c
  emit I32DivS = putWord8 0x6d
  emit I32And = putWord8 0x71
  emit I32Or = putWord8 0x72

instance Emit FloatArith where
  emit F64Sub = putWord8 0x93
  emit F64Add = putWord8 0x92
  emit F64Mul = putWord8 0x94
  emit F64Div = putWord8 0x95

instance Emit IntCmp where
  emit I32LtS = putWord8 0x48
  emit I32LeS = putWord8 0x4c
  emit I32GtS = putWord8 0x4a
  emit I32GeS = putWord8 0x4e
  emit I32Eq = putWord8 0x46
  emit I32Ne = putWord8 0x47

instance Emit FloatCmp where
  emit F64Lt = putWord8 0x5d
  emit F64Le = putWord8 0x5f
  emit F64Gt = putWord8 0x5e
  emit F64Ge = putWord8 0x60
  emit F64Eq = putWord8 0x5b
  emit F64Ne = putWord8 0x5c

instance Emit ValueType where
  emit B = emit I32
  emit I32 = putVarSInt7 $ -0x1
  emit I64 = putVarSInt7 $ -0x2
  emit F32 = putVarSInt7 $ -0x3
  emit F64 = putVarSInt7 $ -0x4
  emit Func = putVarSInt7 $ -0x20
  emit Void = putVarSInt7 $ -0x40

instance Emit ExternalKind where
  emit Function = putVarSInt7 0x0
  emit Table = putVarSInt7 0x1
  emit Memory = putVarSInt7 0x2
  emit Global = putVarSInt7 0x3

instance Emit Export where
  emit (Export name kind idx) = do
    emit name
    emit kind
    emit idx

putAsByteArray :: Put -> Put
putAsByteArray p = do
  let bytes = runPut p
  putVarUInt32 $ fromIntegral $ BL.length bytes
  putLazyByteString bytes

-- TODO: Could this be written cleaner with GADTs?
-- Eg, Emit a => KnownSection -> a
payload :: KnownSection -> Put
payload (Start n) = emit n
payload (Types fs) = emit fs
payload (Funcs is) = emit is
payload (Code bs) = emit bs
payload (Exports es) = emit es

opcode :: KnownSection -> Word8
opcode Start{} = 0x8
opcode Types{} = 0x1
opcode Funcs{} = 0x3
opcode Code{} = 0xa
opcode Exports{} = 0x7

putOpcode :: Word8 -> Put
putOpcode = putWord8

header :: Put
header = do
  putUInt32 magicCookie
  putUInt32 version

magicCookie :: Word32
magicCookie = 0x6d736100

version :: Word32
version = 0x1

putUInt32 :: Word32 -> Put
putUInt32 = putWord32le

putVarUInt1 :: Int8 -> Put
putVarUInt1 = ULEB.putWord8 . fromIntegral

putVarUInt7 :: Int8 -> Put
putVarUInt7 = ULEB.putWord8 . fromIntegral

putVarUInt32 :: Int32 -> Put
putVarUInt32 = ULEB.putWord32 . fromIntegral

putVarUInt64 :: Int64 -> Put
putVarUInt64 = ULEB.putWord64 . fromIntegral

putVarSInt7 :: Int8 -> Put
putVarSInt7 = SLEB.putInt8

putVarSInt32 :: Int32 -> Put
putVarSInt32 = SLEB.putInt32

putVarSInt64 :: Int64 -> Put
putVarSInt64 = SLEB.putInt64

putF64 :: Double -> Put
putF64 = putDoublele
