
module Hash.SHA256
(
)
where

import Data.Word
import Data.Bits
import Numeric
import Data.Char

-- Helper function to display as a bit string
b2 :: (Show a, Integral a) => a -> String
b2 i = showIntAtBase 2 intToDigit i ""


s0 :: Word32 -> Word32
s0 a = (a `rotateR` 2 ) `xor`
       (a `rotateR` 13) `xor`
       (a `rotateR` 22)

s1 :: Word32 -> Word32
s1 e = (e `rotateR` 6 ) `xor` 
       (e `rotateR` 11) `xor` 
       (e `rotateR` 25)

ch :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
ch e f g h = (e .&. f) `xor` ((complement e) .&. g)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj a b c = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)

-- Working variables of the compression function
data CompVars = CompVars Word32 
                         Word32 
                         Word32 
                         Word32 
                         Word32 
                         Word32 
                         Word32 
                         Word32

-- One iteration of compression function
compIter :: CompVars -> Word32 -> Word32 -> CompVars
compIter (CompVars a b c d e f g h) ki wi =
  CompVars a' b' c' d' e' f' g' h'
  where
    t1 = h + s1 e + ch e f g h + ki + wi
    t2 = s0 a + maj a b c

    h' = g
    g' = f
    f' = e
    e' = d+t1
    d' = c
    c' = b
    b' = a
    a' = t1+t2