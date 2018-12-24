
module SHA256.Compression
( Comp(..)
, comp
)
where

import Data.Word
import Data.Bits

import SHA256.Constants
import SHA256.Types


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


-- One iteration of compression function
compIter :: Comp -> Word32 -> Word32 -> Comp
compIter [a, b, c, d, e, f, g, h] ki wi =
  [a', b', c', d', e', f', g', h']
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

comp' :: Integral a => a -> 
         Comp -> 
         (a -> Word32) ->
         (a -> Word32) ->
         Comp
comp' 64 cv _ _ = cv
comp' i  cv k w = comp' (i+1) (compIter cv (k i) (w i)) k w

-- The compression function
comp :: Integral a => Comp -> 
        (a -> Word32) ->
        (a -> Word32) ->
        Comp
comp = comp' 0