
module SHA256.Compression
( compress
)
where

import Data.Word
import Data.Bits

import SHA256.Constants


-- compress ws d. ws is the 64-length message schedule array which
-- will be compressed. d is the current digest, an 8-length array.
compress :: [Word32] -> [Word32] -> [Word32]
compress ws d = zipWith (+) d d'
  where
    d' = compRounds ws d

-- compRounds ws d. ws is the 64-length message schedule array.
-- d is the current digest, an 8-length array.
compRounds :: [Word32] -> [Word32] -> [Word32]
compRounds ws d = foldl (\d' (k,w) -> compRound k w d') d kws
  where
    kws = zip roundConstants ws

-- One round of the compression loop, using the current round
-- constant ki, current message schedule part wi and the
-- current digest.
compRound :: Word32 -> Word32 -> [Word32] -> [Word32]
compRound ki wi [a,b,c,d,e,f,g,h] =
  let t1 = h + s1 e + ch e f g h + ki + wi
      t2 = s0 a + maj a b c

      h' = g
      g' = f
      f' = e
      e' = d+t1
      d' = c
      c' = b
      b' = a
      a' = t1+t2

  in  [a',b',c',d',e',f',g',h']

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