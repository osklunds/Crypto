
module Math.PowerModulo
( powerModulo
)
where

import Test.QuickCheck

-- powerModulo b e m calculates b^e mod m efficiently
powerModulo :: (Integral a) => a -> a -> a -> a
powerModulo b e m
  | b < 0 || e < 0 || m < 2 = error "Bad arguments"
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

newtype BigInt = BigInt Integer 
               deriving Show

instance Arbitrary BigInt where
  arbitrary = do
    i <- choose (2,1000000)
    return $ BigInt i

unwrap :: BigInt -> Integer
unwrap (BigInt i) = i

test_powerModulo :: BigInt -> BigInt -> BigInt -> Bool
test_powerModulo b' e' m' = powerModulo b e m == (b^e `mod` m)
  where
    b = unwrap b'
    e = unwrap e'
    m = unwrap m'