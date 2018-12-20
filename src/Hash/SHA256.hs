
module Hash.SHA256
(
)
where

import Data.Word
import Data.Bits
import Numeric
import Data.Char
--import Test.QuickCheck

import Math.Prime


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
              deriving Show

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


-- Round constants for compression function
k :: Word32 -> Word32
k i = ceiling . 
      (subtract 1) . 
      (*2^32) . 
      (**(1.0/3.0)) .
      fromIntegral $ (primesN :: [Word32]) !! (fromIntegral i)

-- String generated from the calculation
kGeneratedString :: String
kGeneratedString = foldl (\str strs -> str++strs) "" strings
  where
    strings = map si [0..63]
    si i = "0x" ++ showHex (k i) "" ++ ", "

-- Reference string
kTestString :: String
kTestString = "0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0xfc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x6ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2, "

prop_roundConstants :: Bool
prop_roundConstants = kGeneratedString == kTestString


comp' :: Word32 -> 
         CompVars -> 
         (Word32 -> Word32) ->
         (Word32 -> Word32) ->
         CompVars
comp' 64 cv _ _ = cv
comp' i  cv k w = comp' (i+1) (compIter cv (k i) (w i)) k w

-- The compression function
comp :: CompVars -> 
        (Word32 -> Word32) ->
        (Word32 -> Word32) ->
        CompVars
comp = comp' 0
      