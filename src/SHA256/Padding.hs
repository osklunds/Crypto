
module SHA256.Padding
( -- pad
)
where

import Prelude hiding (length, take)
import Data.Word
import Data.Bits
import Numeric

import Math.Divisibility
import Tools
import SHA256.Types


append1bit :: BitString -> BitString
append1bit = (++ [128])

appendZeros :: BitString -> BitString
appendZeros bs
  | 512 `divides` (length bs * 8 + 64) = bs
  | otherwise                          = appendZeros (bs ++ [0])

-- Starts from 0. Only works for non-zero bytes.
msbIndex ::Integral a => Word8 -> a
msbIndex b = last [fromIntegral i | i <- [0..7], testBit b i]

-- There must be no zero-bytes leading.
lengthInBits :: BitString -> Word64
lengthInBits []     = 0
lengthInBits (b:bs) = msbIndex b + 1 + 8 * length bs

-- Represents a 64-bit word as a list of 8-bit words
w64ToW8List :: Word64 -> BitString
w64ToW8List w64 = take 8 [fromIntegral (w64 `shiftR` i) 
                         | i <- [56,48..]]


pad :: BitString -> BitString
pad bs = appendZeros  (append1bit bs) ++ w64ToW8List toUse
  where
    origLen = lengthInBits bs
    
    byteLen = origLen `div` 8
    toUse   = if origLen == byteLen * 8
              then origLen
              else (byteLen+1) * 8


