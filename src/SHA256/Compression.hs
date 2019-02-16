
module SHA256.Compression
( compress
)
where

import Data.Word
import Data.Bits

import SHA256.Types
import SHA256.Constants

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


compRound :: Word32 -> Word32 -> CompVars -> CompVars
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

compRounds :: RoundWords -> RoundWords -> CompVars -> CompVars
compRounds []     []     = id
compRounds (k:ks) (w:ws) = compRounds ks ws . (compRound k w)

compress :: RoundWords -> RoundWords -> Digest -> Digest
compress k w d = zipWith (+) d d'
  where
    d' = compRounds k w d
