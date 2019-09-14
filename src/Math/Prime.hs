
module Math.Prime
( prime
)
where

import System.Random
import Test.QuickCheck
import Prelude hiding (take)
import Data.Bits

import Math.Divisibility
import Math.PowerModulo
import Math.BigInt
import Math.NumClass
import Tools

-- The final prime function. To provide a simpler interface,
-- the randomness is hard-coded.
prime :: NumClass a => a -> Bool
prime n
  | n <  10000 = primeDet n
  | n >= 10000 = fst $ millerRabin n 64 $ mkStdGen 123
  -- A cutoff is used. Below, faster with deterministic. Above,
  -- faster with Miller-Rabin.

primeDet :: Integral a => a -> Bool
primeDet n
  | n < 2  = False
  | n >= 2 = null divisors
  where
    divisors = filter (`divides` n) [2..n-1]

-- millerRabin n k g does k MR rounds testing n for primality.
millerRabin :: (NumClass a, RandomGen g) =>
  a -> a -> g -> (Bool, g)
millerRabin _ 0 g = (True, g)
millerRabin n k g = (this && rest, g'')
  where
    (a, g')     = randomR (1, n-1) g
    this        = millerRabinOnce n a
    (rest, g'') = millerRabin n (k-1) g'

-- millerRabinOnce n a does one MR round on n using a.
millerRabinOnce :: (Bits a, Integral a) => a -> a -> Bool
millerRabinOnce n a
  | even n           = False
  | otherwise        = not (test1 && test2)
  where
    (d,s) = findDS n
    test1 = powerModulo a d n /= 1
    test2 = and $ map (\t -> powerModulo a ((2^t)*d) n /= n-1) 
                      [0..s-1]

-- Finds (d,s) s.t. n-1 = d*2^s, d odd.
findDS :: Integral a => a -> (a, a)
findDS n = head [(d, s) | s <- [0..], 
                        let d = (n-1) `div` (2^s), 
                        n-1 == d*2^s,
                        odd d]

prop_millerRabinOnce :: Bool
prop_millerRabinOnce = and $ map equal [4..5000]
  where
    equal :: Integer -> Bool
    equal n = millerRabinAll n == primeDet n

-- Does MR test on n with all test values a, i.e. a in [1..n-1].
millerRabinAll :: (Bits a, Integral a) => a -> Bool
millerRabinAll n = and $ map (millerRabinOnce n) [1..n-1]

prop_prime :: BigInt5 -> Bool
prop_prime n = prime n == primeDet n