
module SHA256.RepConv
( asciiToW8s
, w8sToW32
, w32sToHex
, w64ToW8List
)
where

import Data.Char
import Data.Word
import Data.Bits
import Prelude hiding (take, length)

import SHA256.Types
import Tools

-- Transforms a string of ascii chars to a string of
-- 8-bit words
asciiToW8s :: String -> ByteString
asciiToW8s = map (fromIntegral . ord)

-- Makes 4 8-bit words into one 32-bit word
w8sToW32 :: [Word8] -> Word32
w8sToW32 = w8sToW32' 24

w8sToW32' :: Int -> ByteString -> Word32
w8sToW32' _ []     = 0
w8sToW32' n (b:bs) = (fromIntegral b) `shiftL` n +
                    w8sToW32' (n-8) bs

-- Appends leading 0s, to always have hex as 8 chars long
leadingZeros :: String -> String
leadingZeros str = take missing (repeat '0') ++ str
  where
    missing = 8 - length str

-- Transforms a digest of 8 32-bit words to a hex string
w32sToHex :: Digest -> String
w32sToHex = concat . map (leadingZeros . toHex)

w64ToW8List :: Word64 -> ByteString
w64ToW8List w64 = [fromIntegral (w64 `shiftR` i) | i <- [56,48..0]]