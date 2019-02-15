
module SHA256.Padding
( pad
)
where

import Prelude hiding (length, take)
import Data.Word
import Data.Bits
import Numeric

import Math.Divisibility
import Tools
import SHA256.Types


appendOne :: BitString -> BitString
appendOne = (++ [128])

appendZeros :: BitString -> BitString
appendZeros bitString
  | isMultiple = bitString
  | otherwise  = appendZeros (bitString ++ [0])
  where
    isMultiple = 512 `divides` (length bitString * 8 + 64)


w64ToW8List :: Word64 -> BitString
w64ToW8List w64 = [fromIntegral (w64 `shiftR` i) | i <- [56,48..0]]

appendLength :: BitString -> BitString -> BitString
appendLength bs = (++ w64ToW8List (8 * length bs))


pad :: BitString -> BitString
pad bs = appendLength bs . appendZeros . appendOne $ bs
