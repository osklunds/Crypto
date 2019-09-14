
module Math.PowerModulo
( powerModulo
)
where

import Test.QuickCheck
import Prelude hiding (length)

import Math.BigInt
import Tools

-- powerModulo b e m calculates b^e modulo m efficiently.
-- Requirements: e>=0 and m>=2.
powerModulo :: Integral a => a -> a -> a -> a
powerModulo b e m = foldr f 1 (toBin e)
  where
    f 0 z = z^2 `mod` m
    f 1 z = f 0 z * b `mod` m

-- Positive base-10 integer to LSB-first bit string.
toBin :: Integral a => a -> [a]
toBin 0 = []
toBin n = let (q, r) = n `quotRem` 2
          in  r:(toBin q)

prop_toBin :: BigInt7 -> Property
prop_toBin n = n >= 0 ==> (==n) . toDec . reverse . toBin $ n

-- MSB-first bit string to positive base-10 integer.
toDec :: Integral a => [a] -> a
toDec = toDec' . reverse

toDec' :: Integral a => [a] -> a
toDec' [] = 0
toDec' (b:bs) = b + 2 * toDec' bs

prop_powerModulo :: BigInt5 -> BigInt5 -> BigInt5 -> Property
prop_powerModulo b e m = e >= 0 && m >= 2 ==> correct
  where
    pm    = powerModulo b e m
    naive = (b^e `mod` m)
    correct = pm == naive && pm >= 0 && pm < m
