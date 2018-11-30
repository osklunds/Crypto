
module Math.PowerModulo
( powerModulo
)
where

import Math.BigInt

import Test.QuickCheck

-- powerModulo b e m calculates b^e mod m efficiently
powerModulo :: (Integral a) => a -> a -> a -> a
powerModulo b e m
  | e < 0 || m < 2 = error "Bad arguments"
  | b < 0                   = powerModulo (b `mod` m) e m
  | e == 0                  = 1
  | b == 0 || b == 1        = b
  | b == m                  = 0
  | b > m                   = powerModulo (b `mod` m) e m
  | b < m                   = (a' * b^d) `mod` m
  where
    (a,k,d) = maximumPower b e m
    a' = powerModulo (a `mod` m) k m

-- maximumPower b e m gives (a,k,d) s.t.
-- 1. b^e = a^k * b^d
-- 2. a = b^x >= m, for minimal x
-- 3. d is minimal
maximumPower :: (Integral a) => a -> a -> a -> (a, a, a)
maximumPower b e m 
  | null list = (1,1,e)
  | otherwise = (a,e `div` x,e `mod` x)
  where
    -- The list of powers and exponents is increasing
    -- so the smallest a is the first element
    list = [(b^x,x) | x <- [1..e], b^x >= m]
    (a,x) = head $ list

prop_powerModulo :: BigInt100000 -> BigInt100000 -> BigInt100000 -> Property
prop_powerModulo b e m = e >= 0 && m >= 2 ==> correct
  where
    r = powerModulo b e m
    correct = r == (b^e `mod` m) && r >= 0 && r < m