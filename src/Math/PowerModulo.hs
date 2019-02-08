
module Math.PowerModulo
( powerModulo
)
where

import Test.QuickCheck
import Prelude hiding (length)

import Math.BigInt
import Tools

-- Positive base-10 integer to LSB-first bit string.
toBin :: Integral a => a -> [a]
toBin 0 = []
toBin n = let (q, r) = n `quotRem` 2
          in  r:(toBin q)

-- MSB-first bit string to positive base-10 integer.
toDec :: Integral a => [a] -> a
toDec = toDec' . reverse

toDec' :: Integral a => [a] -> a
toDec' [] = 0
toDec' (b:bs) = b + 2 * toDec' bs

prop_toBin :: BigInt10000000 -> Property
prop_toBin n = n >= 0 ==> (==n) . toDec . reverse . toBin $ n

-- | powerModulo b e m calculates b^e modulo m efficiently.
powerModulo :: Integral a => a -> a-> a -> a
powerModulo b e m = foldr f 1 bin
  where
    bin = toBin e
    len = length bin

    f 0 z = z^2 `mod` m
    f 1 z = f 0 z * b `mod` m

prop_powerModulo :: BigInt100000 -> BigInt100000 -> BigInt100000 -> Property
prop_powerModulo b e m = e >= 0 && m >= 2 ==> correct
  where
    r = powerModulo b e m
    correct = r == (b^e `mod` m) && r >= 0 && r < m