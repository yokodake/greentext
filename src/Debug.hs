{-# language NamedFieldPuns #-}
module Debug where

import           Code

import           Foreign (Word16, Word8, Int32)
import           Text.Printf

import           Control.Monad.Trans.Writer.Strict (tell, Writer, execWriter)
import qualified Data.ByteString as B
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

-- | print a chunk in human readable format
-- @TODO rewrite all of this: no String, a proper output stream, abstract more of it
disassemble :: String -> Chunk -> L.ByteString
disassemble name c@Chunk{code} = output $ do
  string (printf "=== %s ===\n" name)
  go 0
  where
    caddr i = decode_w16' [code `B.index` (i + 1), code `B.index` (i + 2)]

    go i
      | i == B.length code = pure ()
      | otherwise = do
          let oc = btoc $ code `B.index` i
          instPrefix i oc
          case oc of
            _ | isLit oc -> string (showLit (caddr i) (valSize oc) c)
            _ -> pure ()
          endl
          go (i + operandSize oc + 1)



-- | writes the offset and instruction
instPrefix :: Int -> OpCode -> Writer Builder ()
instPrefix i b = string $ printf "%04x %-8s" i (show b)

string :: String -> Writer Builder ()
string = tell . B.string8

char :: Char -> Writer Builder ()
char = tell . B.char8

endl :: Writer Builder ()
endl = char '\n'

output :: Writer Builder a -> L.ByteString
output = B.toLazyByteString . execWriter

showLit :: Word16 -> Int -> Chunk -> String
showLit addr size = show' . B.take size . B.drop (fromIntegral addr) . constants
  where
    show' = case size of
              8 -> show . decode @Double
              4 -> show . decode @Int32
              1 -> show . decode @Word8
              _ -> ("???" <>) . show
