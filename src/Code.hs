{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE CPP                        #-}
module Code where

import           Prelude         hiding (init)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Coerce
import           Foreign         (Int16, Int32, Int64, Ptr, Storable, Word16,
                                  Word8)
import qualified Foreign         as F
import           GHC.IO          (unsafePerformIO)
import           Text.Printf     (printf)

import           Ast             (Sym)

-- TODO : global vars

-- * Chunks
data Chunk = MkChunk { name  :: Sym    -- ^ chunk name ~ function name
                     , start :: !IAddr
                     , code  :: !ByteString
                     }

-- | literals
newtype SMem = MkSMem ByteString
  deriving (Show, Eq)
  deriving newtype (Semigroup, Monoid)


-- | \@TODO: global vars
data Module = MkModule { constants :: !SMem    -- ^ static mem for literals
                       , funcs     :: [Chunk]  -- ^ list of functions
                       }

-- | initialize a new chunk
init :: String -> IAddr -> Chunk
init name start = MkChunk { name, start, code= mempty}

-- * OPCODE
type Instr = Word8
-- | instr. addr
newtype IAddr = MkIAddr Word16
  deriving Num via Word16
instance Show IAddr where
  show (MkIAddr x) = printf "%%0x%04x" x
-- | addr in static mem.
newtype SAddr = MkSAddr Word16
  deriving Num via Word16
instance Show SAddr where
  show (MkSAddr x) = printf "*0x%04x" x


-- | size of a static memory address
sasize :: Int
sasize = let (MkSAddr x) = undefined in F.sizeOf x

-- | size of a instruction address
iasize :: Int
iasize = let (MkIAddr x) = undefined in F.sizeOf x

-- | add a constant to the constants array
addC :: Marshal a => a -> SMem -> SMem
addC v sm = coerce B.append sm (encode v)

#define OPCODE_TABLE(F) \
  F(=, Ipop    , {- ^ @POP [x]@ pop value from stack -}               , "POP"    , 0) \
  F(|, Iret    , {- ^ @RET    @ return form function -}               , "RET"    , 0) \
  F(|, Irettop , {- ^ @RET [x]@ return (and pops) top of the stack -} , "RETTOP" , 0) \
  F(|, IloadRet,{- ^ @LDR    @ push return value on stack -}          , "LOADRET", 0) \
  F(|, Iload  , {- ^ @LDS $x @ push slot (variables) on stack -}      , "LOAD"   , 0) \
  F(|, Istore , {- ^ @STR    @ put top of stack in a slot (Var) -}    , "STORE"  , 0) \
\
  F(|, Ilit1  , {- ^ @LIT *x@ 1 byte:  constant bool -}           , "LIT1" , sasize) \
  F(|, Ilit4  , {- ^ @LIT *x@ 4 bytes: constant signed integer -} , "LIT4" , sasize) \
  F(|, Ilit8  , {- ^ @LIT *x@ 8 bytes: constant double -}         , "LIT8" , sasize) \
  F(|, IlitS  , {- ^ @LIT *x@ N bytes: constant string -}         , "LITS" , sasize) \
\
  F(|, Iand   , {- ^ @AND [x;y]@ infix operator @and@ -} , "OP_and", 0) \
  F(|, Ior    , {- ^ @OR  [x;y]@ infix operator @or@ -}  , "OP_or" , 0) \
  F(|, Igt    , {- ^ @GT  [x;y]@ infix operator @>@ -}   , "OP_>"  , 0) \
  F(|, Ige    , {- ^ @GE  [x;y]@ infix operator @>=@ -}  , "OP_>=" , 0) \
  F(|, Ilt    , {- ^ @LT  [x;y]@ infix operator @<@ -}   , "OP_<"  , 0) \
  F(|, Ile    , {- ^ @LE  [x;y]@ infix operator @<=@ -}  , "OP_<=" , 0) \
  F(|, Ieq    , {- ^ @EQ  [x;y]@ infix operator @==@ -}  , "OP_==" , 0) \
  F(|, Ineq   , {- ^ @NEQ [x;y]@ infix operator @!=@ -}  , "OP_!=" , 0) \
  F(|, Iadd   , {- ^ @ADD [x;y]@ infix operator @+@ -}   , "OP_+"  , 0) \
  F(|, Imin   , {- ^ @MIN [x;y]@ infix operator @-@ -}   , "OP_-"  , 0) \
  F(|, Imul   , {- ^ @MUL [x;y]@ infix operator @*@ -}   , "OP_*"  , 0) \
  F(|, Idiv   , {- ^ @DIV [x;y]@ infix operator @/@ -}   , "OP_/"  , 0) \
  F(|, Imod   , {- ^ @MOD [x;y]@ infix operator @%@ -}   , "OP_%"  , 0) \
\
  F(|, Ibrf   , {- ^ @BRF %p [x]@ branch if true -}      , "BRF", iasize) \
  F(|, Ibrt   , {- ^ @BRT %p [x]@ branch if false -}     , "BRT", iasize) \
  F(|, Ijmp   , {- ^ @JMP %p    @ unconditional jump -}  , "JMP", iasize) \
\
  F(|, Icall  , {- ^ @CALL %p@ function call -}          , "CALL", iasize) \
  F(|, Iexit  , {- ^ @EXIT@    exit program  -}          , "EXIT", 0)

#define OPCODE_TYPE(d, dtor, comment, _1, _2) d dtor comment
#define OPCODE_SHOW(_, dtor, _1, str, _2) show dtor = str;
#define OPCODE_SIZE(_, dtor, _1, _2, size) operandSize dtor = size;

{- | notation:

   * @[x,y]@ indicates values on stack: x top of stack, y 2nd on stack

   * @$x@    stack pointer\n

   * @*x@    data pointer

   * @%x@    code pointer
-}
data OpCode OPCODE_TABLE(OPCODE_TYPE) deriving (Enum)

instance Show OpCode where { OPCODE_TABLE(OPCODE_SHOW) } 

-- | get size of the instr's operand in bytes
operandSize :: OpCode -> Int
OPCODE_TABLE(OPCODE_SIZE)

-- | code to byte
ctob :: OpCode -> Instr
ctob = fromIntegral . fromEnum
{-# INLINE ctob #-}

-- | byte to code
btoc :: Instr -> OpCode
btoc = toEnum . fromIntegral
{-# INLINE btoc #-}


-- | size of the value
valSize :: OpCode -> Int
valSize Ilit1 = 1
valSize Ilit4 = 4
valSize Ilit8 = 8
valSize _     = 0
{-# INLINE valSize #-}

-- | is a Literal
isLit :: OpCode -> Bool
isLit Ilit1 = True
isLit Ilit4 = True
isLit Ilit8 = True
isLit _     = False
{-# INLINE isLit #-}

-- ** Marshalling
class Marshal a where
  encode' :: a -> [Word8]
  decode' :: [Word8] -> a

  encode :: a -> ByteString
  encode = B.pack . encode'
  decode :: ByteString -> a
  decode = decode' . B.unpack
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
instance Marshal Bool where
  encode' True  = encode_w8' 0
  encode' False = encode_w8' 1
  decode' [0] = False
  decode' [_] = True
  decode' _   = error ("Code.Marshal.decode' not one byte.")

encode_w8 :: Word8 -> ByteString
encode_w8' :: Word8 -> [Word8]
encode_w16 :: Word16 -> ByteString
encode_w16' :: Word16 -> [Word8]
encode_w8 = B.singleton
encode_w16 = B.pack . encode_w16'
encode_w8' = pure
encode_w16' !w = unsafePerformIO $ F.with w (F.peekArray (sizeof @Word16) . castptr @Word8)
decode_w8 :: ByteString -> Word8
decode_w8' :: [Word8] -> Word8
decode_w16 :: ByteString -> Word16
decode_w16' :: [Word8] -> Word16
decode_w8 = B.head
decode_w8' = head
decode_w16 = decode_w16' . B.unpack
decode_w16' ws = unsafePerformIO $ F.withArray ws (F.peek . castptr @Word16)

encode_f32  :: Float -> ByteString
encode_f32' :: Float -> [Word8]
encode_f64  :: Double -> ByteString
encode_f64' :: Double -> [Word8]
encode_f32 = B.pack . encode_f32'
encode_f64 = B.pack . encode_f64'
encode_f32' !f = unsafePerformIO $ F.with f (F.peekArray (sizeof @Float)  . castptr @Word8)
encode_f64' !d = unsafePerformIO $ F.with d (F.peekArray (sizeof @Double) . castptr @Word8)

encode_i16  :: Int16 -> ByteString
encode_i16' :: Int16 -> [Word8]
encode_i32  :: Int32 -> ByteString
encode_i32' :: Int32 -> [Word8]
encode_i64  :: Int64 -> ByteString
encode_i64' :: Int64 -> [Word8]
encode_i16  = B.pack . encode_i16'
encode_i32  = B.pack . encode_i32'
encode_i64  = B.pack . encode_i64'
encode_i16' !i = unsafePerformIO $ F.with i (F.peekArray (sizeof @Int16) . castptr @Word8)
encode_i32' !i = unsafePerformIO $ F.with i (F.peekArray (sizeof @Int32) . castptr @Word8)
encode_i64' !i = unsafePerformIO $ F.with i (F.peekArray (sizeof @Int64) . castptr @Word8)

decode_f32  :: ByteString -> Float
decode_f32' :: [Word8] -> Float
decode_f64  :: ByteString -> Double
decode_f64' :: [Word8] -> Double
decode_f32 = decode_f32' . B.unpack
decode_f64 = decode_f64' . B.unpack
decode_f32' !ws = unsafePerformIO $ F.withArray ws (F.peek . castptr @Float )
decode_f64' !ws = unsafePerformIO $ F.withArray ws (F.peek . castptr @Double)

decode_i16  :: ByteString -> Int16
decode_i16' :: [Word8] -> Int16
decode_i32  :: ByteString -> Int32
decode_i32' :: [Word8] -> Int32
decode_i64  :: ByteString -> Int64
decode_i64' :: [Word8] -> Int64
decode_i16 = decode_i16' . B.unpack
decode_i32 = decode_i32' . B.unpack
decode_i64 = decode_i64' . B.unpack
decode_i16' ws = unsafePerformIO $ F.withArray ws (F.peek . castptr @Int16)
decode_i32' ws = unsafePerformIO $ F.withArray ws (F.peek . castptr @Int32)
decode_i64' ws = unsafePerformIO $ F.withArray ws (F.peek . castptr @Int64)

-- * runtime rep
data TypeTag = Bool  | Int  | Double  | String  -- normal values
             | LBool | LInt | LDouble | LString -- literal values

instance Enum TypeTag where
  fromEnum Bool    = 0b000
  fromEnum Int     = 0b001
  fromEnum Double  = 0b010
  fromEnum String  = 0b011
  fromEnum LBool   = 0b100
  fromEnum LInt    = 0b101
  fromEnum LDouble = 0b110
  fromEnum LString = 0b111
  toEnum 0b000 = Bool
  toEnum 0b001 = Int
  toEnum 0b010 = Double
  toEnum 0b011 = String
  toEnum 0b100 = LBool
  toEnum 0b101 = LInt
  toEnum 0b110 = LDouble
  toEnum 0b111 = LString

-- * utilities
-- | reverse the type variables so the first type applied arg is @b@,
-- @a@ will almost always be inferrred from the function arg anyways.
castptr :: forall b a. Ptr a -> Ptr b
castptr = F.castPtr

-- | sizeof as a polymorphic variable instead, way better with @-XTypeApplications@
-- than passing a dummy term, also much more succint for some cases
sizeof :: forall a. Storable a => Int
sizeof = F.sizeOf (undefined :: a)
