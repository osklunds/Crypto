
module Math.PowerModulo
( powerModulo
)
where

import Test.QuickCheck
import Prelude hiding (length)
import Data.Bits

import Math.BigInt
import Tools

-- powerModulo b e m calculates b^e modulo m efficiently.
-- Requirements: e>=0 and m>=2.
powerModulo :: (Bits a, Integral a) => a -> a -> a -> a
powerModulo b e m = foldr f 1 (toBits e)
  where
    f False z = z^2     `mod` m
    f True  z = z^2 * b `mod` m

-- Converts an integer to an LSB-first bitstream.
toBits :: Bits a => a -> [Bool]
toBits n = includeTrues bits bitLen
  where
    bits   = map (testBit n) [0..]
    bitLen = popCount n

-- Takes from the bitsream until the amount of Trues has been included.
includeTrues :: [Bool] -> Int -> [Bool]
includeTrues _          0 = []
includeTrues (True:bs)  n = True:(includeTrues bs $ n-1)
includeTrues (False:bs) n = False:(includeTrues bs n)

prop_toBits :: BigInt7 -> Property
prop_toBits n = n >= 0 ==> toInt (toBits n) == n

toInt :: Integral a => [Bool] -> a
toInt bs = foldr f 0 bs
  where
    f False sum = sum*2
    f True  sum = sum*2+1

prop_powerModulo :: BigInt5 -> BigInt5 -> BigInt5 -> Property
prop_powerModulo b e m = e >= 0 && m >= 2 ==> correct
  where
    pm    = powerModulo b e m
    naive = (b^e `mod` m)
    correct = pm == naive && pm >= 0 && pm < m