
module SHA256.RepConv
( asciiToW8list
, w8x4ToW32
, w32x8ToHex
, w64ToW8x8
)
where

import Data.Char
import Data.Word
import Data.Bits
import Prelude hiding (take, length)

import SHA256.Types
import Tools

asciiToW8list :: String -> ByteString
asciiToW8list = map (fromIntegral . ord)

w8x4ToW32 :: [Word8] -> Word32
w8x4ToW32 = w8x4ToW32' 24

w8x4ToW32' :: Int -> ByteString -> Word32
w8x4ToW32' _ []     = 0
w8x4ToW32' n (b:bs) = (fromIntegral b) `shiftL` n +
                      w8x4ToW32' (n-8) bs

prependZeros :: String -> String
prependZeros str = replicate missing '0' ++ str
  where
    missing = 8 - length str

w32x8ToHex :: Digest -> String
w32x8ToHex = concat . map (prependZeros . toHex)

w64ToW8x8 :: Word64 -> ByteString
w64ToW8x8 w64 = [fromIntegral (w64 `shiftR` i) | i <- [56,48..0]]