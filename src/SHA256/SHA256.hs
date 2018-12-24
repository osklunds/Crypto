
module SHA256.SHA256
(
)
where

import Data.Word
import Data.Bits
import Numeric
import Data.Char
import Test.QuickCheck hiding ((.&.))

import Math.Prime
import Math.Common
import Math.BigInt




    

-- List of the message schedule array
ws :: [Word32] -> [Word32]
ws cnk = list
  where
    list = cnk ++ map w [16..63]

    w i = w16 + s0 + w7 + s1
      where
        w16 = list !! (i-16)
        w15 = list !! (i-15)
        w7  = list !! (i-7)
        w2  = list !! (i-2)

        s0 = (w15 `rotateR` 7 ) `xor`
             (w15 `rotateR` 18) `xor`
             (w15 `shiftR`  3 )
        s1 = (w2  `rotateR` 17) `xor`
             (w2  `rotateR` 19) `xor`
             (w2  `shiftR`  10)

-- Message schedule array as a function
wc :: [Word32] -> Word32 -> Word32
wc cnk i = ws cnk !! (fromIntegral i)


-- Merkle-Damgård. Given a compression function
-- taking a tag and a message, this gives
-- a function taking arbitrarily long list
-- of messages, using the compression function
-- and making it into a tag
-- Need to supply initial tag/iv.
merkleDamgard :: (t -> m -> t) -> t -> ([m] -> t)
merkleDamgard _ t []     = t
merkleDamgard h t (m:ms) = merkleDamgard h (h t m) ms


-- The type of the tag used in the MD function
newtype MDTag = MDTag [Word32] -- length = 8
              deriving Show

-- The type of the message used in the MD function
newtype MDMes = MDMes [Word32] -- length = 16
              deriving Show




mdIV :: MDTag
mdIV = MDTag $ map k [0..7]

mdComp :: MDTag -> MDMes -> MDTag
mdComp (MDTag [a,b,c,d,e,f,g,h]) (MDMes ms) =
  MDTag [a',b',c',d',e',f',g',h']
  where
    cv = Comp a b c d e f g h
    (Comp a' b' c' d' e' f' g' h') = comp cv k (wc ms)

inner :: [MDMes] -> MDTag
inner = merkleDamgard mdComp mdIV


-- Returns the index of the most significant bit.
-- Starting from 0.
msb :: Integral a => a -> a
msb n
  | n < 0     = error "Negative number"
  | otherwise = fromIntegral $ msb' 0
  where
    msb' i
      | n `div` 2^i == 0 = i-1
      | otherwise        = msb' (i+1)

prop_msb :: Integer -> Property
prop_msb n = n > 0 && n < 12 ==> 
  and $ map ((==n).msb) [2^n..2^(n+1)-1]


-- Gives the index of the first bit where the length
-- should be placed, based on the msb of the rest
-- of the string
lenStart :: Integral a => a -> a
lenStart msb
  | remain >  64 = ((msb `div` 512)+1)*512 - 64
  | otherwise    = ((msb `div` 512)+2)*512 - 64
  where
    remain = 512 - msb `mod` 512

prop_lenStart :: BigInt10000000 -> Property
prop_lenStart msb = msb > 0 ==> 
  512 `divides` (ls+64) && ls > msb && ls-msb <= 512
  where
    ls = lenStart msb


pad :: (Bits a, Integral a) => a -> a
pad n = n' + lShifted
  where
    lenOrig = msb n + 1
    n'  = setBit n (fromIntegral lenOrig)
    highestBit = lenOrig

    ls = lenStart highestBit
    lShifted = lenOrig `shiftL` (fromIntegral $ ls+1)

-- Tests that the end of the padded is the original
-- string
prop_padEnd :: BigInt10000000 -> Property
prop_padEnd n = n > 0
  ==> padded `mod` 2^len == n
  where
    padded = pad n
    len    = msb n + 1

-- Tests that the extra '1' bit got included
prop_padExtra :: BigInt10000000 -> Property
prop_padExtra n = n > 0
  ==> padded `mod` 2^(len+1) == n + 2^len
  where
    padded = pad n
    len    = msb n + 1

prop_padLength :: BigInt10000000 -> Property
prop_padLength n = n > 0
  ==> padded `div` 2^(lenStart len + 1) == len
  where
    padded = pad n
    len    = msb n + 1


