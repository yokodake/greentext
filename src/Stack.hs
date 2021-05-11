{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples   #-}
module Stack ( pattern Stack 
             , Stack
             , Value(..)
             , push_bs
             , pop_bs 
             ) where

import           Prelude                  hiding (drop, take)

import           Data.ByteString          (drop, take)
import           Data.ByteString.Internal (ByteString (..))
import           Data.Coerce              (coerce)
import           Data.Word                (Word8)
import           Foreign.ForeignPtr       (ForeignPtr)

-- | The reason to use ByteString is for variable sized elements. 
-- One advantage is that only pushing on the Stack incurs a copy, popping just changes the length field.
newtype Stack = S ByteString 
              deriving (Show, Eq)

pattern Stack :: ForeignPtr Word8 -> Int -> Stack
pattern Stack fp len = S (BS fp len)
{-# COMPLETE Stack #-}

class Value a where
    push :: a -> Stack -> Stack
    pop :: Stack -> (# Stack, a #)

-- | O(n+m) push a ByteString on the stack.
push_bs :: ByteString -> Stack -> Stack
push_bs (BS _ 0) s     = s
push_bs bs (Stack _ 0) = S bs
push_bs bs (S bs')     = S (bs <> bs')

-- | O(1) pop variable size elements in the form of a @ByteString@
pop_bs :: Int -> Stack -> (# Stack, ByteString #)
pop_bs size s@(Stack _ len) = (# coerce drop (len - size) s
                               , coerce take (len - size) s #)