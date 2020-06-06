{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language AllowAmbiguousTypes #-}
{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language NamedFieldPuns #-}
module Code where

import           Prelude hiding (init)

import           Foreign (Ptr, Word8, Word16, Int32, Int64, Int16, Storable)
import qualified Foreign as F
import           GHC.IO (unsafePerformIO)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L


-- == TODO
--   - unboxed types and shit
--   - unified load parameter abstracted over size for constants?
--   - decide on address size

-- = Chunks
data Chunk = Chunk { code :: ByteString
                   , constants :: ByteString
                   }

-- | initialize a new chunk
init :: Chunk
init = Chunk mempty mempty

-- | write an instr at end of chunk
write :: Instr -> Chunk -> Chunk
write i c@Chunk{code} = c{code=B.snoc code i}

type Addr = Int16

-- | adress size of a constant value
casize :: Int
casize = sizeof @Addr


-- | add a constant to the constants array
addC :: Marshal a => a -> Chunk -> Chunk
addC v c@Chunk{constants} = c{constants=constants `B.append` encode v}

-- = OPCODE
type Instr = Word8

data OpCode = Iret
            | Ilit1 -- ^ 1 byte,  constant bool
            | Ilit4 -- ^ 4 bytes, constant signed integer
            | Ilit8 -- ^ 8 bytes, constant double
            deriving (Show, Enum)

ctob :: OpCode -> Instr
ctob = fromIntegral . fromEnum

btoc :: Instr -> OpCode
btoc = toEnum . fromIntegral

operandSize :: OpCode -> Int
operandSize Iret = 0
operandSize Ilit1 = casize
operandSize Ilit4 = casize
operandSize Ilit8 = casize

-- == Marshalling
class Marshal a where
  encode :: a -> ByteString
  encode = B.pack . encode'
  encode' :: a -> [Word8]
  decode :: ByteString -> a
  decode = decode' . B.unpack
  decode' :: [Word8] -> a
  {-# MINIMAL encode', decode' #-}

instance Marshal Double where
  encode' = encode_f64'
  decode' = decode_f64'
instance Marshal Int32 where
  encode' = encode_i32'
  decode' = decode_i32'
instance Marshal Word8 where
  encode' = encode_w8'
  decode' = decode_w8'
instance Marshal Word16 where
  encode' = encode_w16'
  decode' = decode_w16'


encode_w8 :: Word8 -> ByteString
encode_w8' :: Word8 -> [Word8]
encode_w16 :: Word16 -> ByteString
encode_w16' :: Word16 -> [Word8]
encode_w8 = B.singleton
encode_w16 = B.pack . encode_w16'
encode_w8' = pure
encode_w16' !w = unsafePerformIO $ F.with w (\buf -> F.peekArray (sizeof @Word16) (castptr @Word8 buf))
decode_w8 :: ByteString -> Word8
decode_w8' :: [Word8] -> Word8
decode_w16 :: ByteString -> Word16
decode_w16' :: [Word8] -> Word16
decode_w8 = B.head
decode_w8' = head
decode_w16 = decode_w16' . B.unpack
decode_w16' ws = unsafePerformIO $ F.withArray ws (\buf -> F.peek (castptr @Word16 buf))

encode_f32  :: Float -> ByteString
encode_f32' :: Float -> [Word8]
encode_f64  :: Double -> ByteString
encode_f64' :: Double -> [Word8]
encode_f32 = B.pack . encode_f32'
encode_f64 = B.pack . encode_f64'
encode_f32' !f = unsafePerformIO $ F.with f (\buf -> F.peekArray (sizeof @Float) (castptr @Word8 buf))
encode_f64' !d = unsafePerformIO $ F.with d (\buf -> F.peekArray (sizeof @Double) (castptr @Word8 buf))

encode_i16  :: Int16 -> ByteString
encode_i16' :: Int16 -> [Word8]
encode_i32  :: Int32 -> ByteString
encode_i32' :: Int32 -> [Word8]
encode_i64  :: Int64 -> ByteString
encode_i64' :: Int64 -> [Word8]
encode_i16  = B.pack . encode_i16'
encode_i32  = B.pack . encode_i32'
encode_i64  = B.pack . encode_i64'
encode_i16' !i = unsafePerformIO $ F.with i (\buf -> F.peekArray (sizeof @Int16) (castptr @Word8 buf))
encode_i32' !i = unsafePerformIO $ F.with i (\buf -> F.peekArray (sizeof @Int32) (castptr @Word8 buf))
encode_i64' !i = unsafePerformIO $ F.with i (\buf -> F.peekArray (sizeof @Int64) (castptr @Word8 buf))

decode_f32  :: ByteString -> Float
decode_f32' :: [Word8] -> Float
decode_f64  :: ByteString -> Double
decode_f64' :: [Word8] -> Double
decode_f32 = decode_f32' . B.unpack
decode_f64 = decode_f64' . B.unpack
decode_f32' !ws = unsafePerformIO $ F.withArray ws (\buf -> F.peek (castptr @Float buf))
decode_f64' !ws = unsafePerformIO $ F.withArray ws (\buf -> F.peek (castptr @Double buf))

decode_i16  :: ByteString -> Int16
decode_i16' :: [Word8] -> Int16
decode_i32  :: ByteString -> Int32
decode_i32' :: [Word8] -> Int32
decode_i64  :: ByteString -> Int64
decode_i64' :: [Word8] -> Int64
decode_i16 = decode_i16' . B.unpack
decode_i32 = decode_i32' . B.unpack
decode_i64 = decode_i64' . B.unpack
decode_i16' ws = unsafePerformIO $ F.withArray ws (\buf -> F.peek (castptr @Int16 buf))
decode_i32' ws = unsafePerformIO $ F.withArray ws (\buf -> F.peek (castptr @Int32 buf))
decode_i64' ws = unsafePerformIO $ F.withArray ws (\buf -> F.peek (castptr @Int64 buf))

-- * utilities
-- | reverse the type variables so the first type applied arg is @b@
-- @a@ will almost always be inferrred from the function arg anyways.
castptr :: forall b a. Ptr a -> Ptr b
castptr = F.castPtr

-- | sizeof as a polymorphic variable instead, way better with @-XTypeApplications@
-- than passing a dummy term, also much more succint for some cases
sizeof :: forall a. Storable a => Int
sizeof = F.sizeOf (undefined :: a)
