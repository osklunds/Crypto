
module SHA256.RepConv
( asciiToW8list
, w8x4ToW32
, w64ToW8x8
, w32x8ToHex
)
where

import Data.Char
import Data.Word
import Data.Bits
import Prelude hiding (take, length)

import Tools

-- In all functions, MSB is considered.

asciiToW8list :: String -> [Word8]
asciiToW8list = map (fromIntegral . ord)


w8x4ToW32 :: [Word8] -> Word32
w8x4ToW32 = w8x4ToW32' 24

w8x4ToW32' :: Int -> [Word8] -> Word32
w8x4ToW32' _ []     = 0
w8x4ToW32' n (b:bs) = (fromIntegral b) `shiftL` n + w8x4ToW32' (n-8) bs


w64ToW8x8 :: Word64 -> [Word8]
w64ToW8x8 w64 = [fromIntegral (w64 `shiftR` i) | i <- [56,48..0]]


w32x8ToHex :: [Word32] -> String
w32x8ToHex = concat . map (prependZeros . toHex)

prependZeros :: String -> String
prependZeros str = replicate missing '0' ++ str
  where
    missing = 8 - length str