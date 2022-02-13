module Wasm.Emit (emit) where

import Wasm.Types

import Data.Binary.Put
import qualified Data.Binary.SLEB128 as SLEB
import qualified Data.Binary.ULEB128 as ULEB
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word

class Emit a where
  emit :: a -> Put

instance Emit Program where
  emit (Program ms) = do
    header
    emit $ head ms

instance Emit Module where
  emit (Module sections) =
    mapM_ emit sections

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

instance Emit Basic where
  emit Nop = do
    putWord8 0x1

instance Emit Cf where
  emit End = do
    putWord8 0xb

instance Emit ValueType where
  emit B = emit I32
  emit I32 = putVarSInt7 $ -0x1
  emit I64 = putVarSInt7 $ -0x2
  emit F32 = putVarSInt7 $ -0x3
  emit F64 = putVarSInt7 $ -0x4
  emit Func = putVarSInt7 $ -0x20

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

opcode :: KnownSection -> Word8
opcode Start{} = 0x8
opcode Types{} = 0x1
opcode Funcs{} = 0x3
opcode Code{} = 0xa

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
