
module SHA256.RepConv
( asciiToW8
, w8ToW32
, w8ToHex
)
where

import Data.Char
import Data.Word
import Data.Bits

import SHA256.Types
import Tools

-- Transforms a string of ascii chars to a string of
-- 8-bit words
asciiToW8 :: String -> BitString
asciiToW8 = map (fromIntegral . ord)

-- Makes 4 8-bit words into one 32-bit word
w8ToW32 :: [Word8] -> Word32
w8ToW32 = w8ToW32' 24

w8ToW32' :: Int -> BitString -> Word32
w8ToW32' _ []     = 0
w8ToW32' n (b:bs) = (fromIntegral b) `shiftL` n +
                    w8ToW32' (n-8) bs

-- Transforms a digest of 8-bit words to a hex string
w8ToHex :: Digest -> String
w8ToHex = concat . map toHex