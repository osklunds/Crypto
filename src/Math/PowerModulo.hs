
-- Modular exponentiation done efficiently.

module Math.PowerModulo
( powerModulo
)
where

import Test.QuickCheck
import Prelude hiding (length)

import Math.BigInt
import Tools


-- Positive integer to MSB bit string
toBin :: Integral a => a -> [a]
toBin = reverse . toBin'

toBin' :: Integral a => a -> [a]
toBin' 0 = []
toBin' n = let (q, r) = n `quotRem` 2
          in  r:(toBin' q)

toDec :: Integral a => [a] -> a
toDec = toDec' . reverse

toDec' :: Integral a => [a] -> a
toDec' [] = 0
toDec' (b:bs) = b + 2 * toDec' bs

prop_toBin :: BigInt10000000 -> Property
prop_toBin n = n >= 0 ==> n == (toDec $ toBin n)


-- powerModulo b e m calculates b^e `mod` m efficiently.
powerModulo :: Integral a => a -> a-> a -> a
powerModulo b e m = foldl f 1 bin
  where
    bin = toBin e
    len = length bin

    f z 0 = z^2 `mod` m
    f z 1 = f z 0 * b `mod` m

prop_powerModulo :: BigInt100000 -> BigInt100000 -> BigInt100000 -> Property
prop_powerModulo b e m = e >= 0 && m >= 2 ==> correct
  where
    r = powerModulo b e m
    correct = r == (b^e `mod` m) && r >= 0 && r < m