
module SHA256.Inner
( comp
)
where

import Control.Monad.State
import Control.Monad.State.Lazy
import Data.Word
import Data.Bits

import SHA256.Constants
--import SHA256.Types


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


type Digest = (Word32,
               Word32,
               Word32,
               Word32,
               Word32,
               Word32,
               Word32,
               Word32)

digestAdd :: Digest -> Digest -> Digest
digestAdd (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) =
  (a1+a2,b1+b2,c1+c2,d1+d2,e1+e2,f1+f2,g1+g2,h1+h2)

compRound :: Word32 -> Word32 -> State Digest ()
compRound ki wi = state $ \(a,b,c,d,e,f,g,h) ->
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

  in  ((),(a',b',c',d',e',f',g',h'))

comp :: [Word32] -> [Word32] -> State Digest ()
comp []     []     = return ()
comp (k:ks) (w:ws) = compRound k w >>= 
                     (\_ -> comp ks ws)
