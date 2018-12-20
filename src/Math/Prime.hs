
module Math.Prime
( millerRabin
, prime
, prime100
, prime1000
, prime10000
, primes
, primesN
)
where

import System.Random
import Test.QuickCheck

import Math.Common
import Math.PowerModulo
import Math.BigInt

primeNaive :: Integral a => a -> Bool
primeNaive n
  | n < 2  = False
  | n >= 2 = null [d | d <- [2..n-1], d `divides` n]

-- Finds d and s for n, used in MR
findDS :: Integral a => a -> (a, a)
findDS n = findDS' (n-1) 0
  where
    findDS' q s
      | even q = findDS' (q `div` 2) (s+1)
      | odd  q = (q,s)

-- Does one MR round for an a
millerRabinOnce :: Integral a => a -> a -> a -> a -> Bool
millerRabinOnce n a d s
  | even n           = False
  | otherwise        = not (test1 && test2)
  where
    (d,s) = findDS n

    test1 = powerModulo a d n /= 1
    test2 = and $ map (\t -> powerModulo a ((2^t)*d) n /= n-1) 
                      [0..s-1]

-- Does MR test for all a
millerRabinAll :: Integral a => a -> Bool
millerRabinAll n
  | n <= 3    = error "Too small for MR"
  | otherwise = and $ map (\a -> millerRabinOnce n a d s) [1..n-1]
  where
    (d,s) = findDS $ n

-- Tests numbers up to k that MR test is correct
testMillerRabinAll :: Integral a => a -> Bool
testMillerRabinAll k = and $ map (\n -> millerRabinAll n == 
                                 primeNaive n) [4..k]

-- General MR test
millerRabin :: (RandomGen g, Random a, Integral a) => a -> a -> 
                                                 g -> (Bool, g)
millerRabin n k gen = millerRabin' n k gen
  where
    (d,s)    = findDS n

    millerRabin' _ 0 gen = (True, gen)
    millerRabin' n k gen
      | cand      = millerRabin' n (k-1) gen'
      | otherwise = (False, gen')
      where
        (a,gen')  = randomR (1, n-1) gen
        cand      = millerRabinOnce n a d s

-- Does k MR rounds
primek :: (Integral a, Random a) => a -> a -> Bool
primek k n
  | n < 2            = False
  | n == 2 || n == 3 = True
  | otherwise        = fst $ millerRabin n k' (mkStdGen 1337)
  where
    k' = min n k

-- Short forms
prime100, prime1000, prime10000, prime :: (Integral a, Random a)
                                       => a -> Bool
prime100   = primek 100
prime1000  = primek 1000
prime10000 = primek 10000
prime      = prime100

prop_prime :: BigInt100000 -> Bool
prop_prime n = prime n == primeNaive n

-- List of all primes
primes :: (Random a, Integral a) => [a]
primes = filter prime [0..]

-- List of all primes. Faster for small numbers, but
-- much slower for larger ones.
primesN :: Integral a => [a]
primesN = filter primeNaive [0..]