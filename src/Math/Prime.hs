
-- | Module for primality testing and list of primes.
-- Both probabilistic (fast but cumbersome) and
-- deterministic testing is provided.

module Math.Prime
( 
  -- * Probabilistic tests.
  prime
, primeK
, primes
  -- * Deterministic tests.
, primeDet
, primesDet
)
where

import System.Random
import Control.Monad.State
import Test.QuickCheck

import Math.Common
import Math.PowerModulo
import Math.BigInt
import Math.Random


-- | Deterministic primality test.
primeDet :: Integral a => a -> Bool
primeDet n
  | n < 2  = False
  | n >= 2 = null . filter (`divides` n) $ [2..n-1]

-- | List of all primes deterministically determined.
primesDet :: Integral a => [a]
primesDet = filter primeDet [0..]


-- | findDS n, for odd n, gives odd d and s >= 0 s.t. n=2^s*d.
findDS :: Integral a => a -> (a, a)
findDS n = findDS' (n-1) 0
  where
    findDS' q s
      | even q = findDS' (q `div` 2) (s+1)
      | odd  q = (q,s)

-- | millerRabinOnce n d s a does one MR round test on
-- n using a.
millerRabinOnce :: Integral a => a -> a -> a -> a -> Bool
millerRabinOnce n d s a
  | even n           = False
  | otherwise        = not (test1 && test2)
  where
    (d,s) = findDS n

    test1 = powerModulo a d n /= 1
    test2 = and $ map (\t -> powerModulo a ((2^t)*d) n /= n-1) 
                      [0..s-1]

-- | Does MR test of n with all test values a for n.
millerRabinAll :: Integral a => a -> Bool
millerRabinAll n
  | n <= 3    = error "Too small for Miller-Rabin"
  | otherwise = and $ map (millerRabinOnce n d s) [1..n-1]
  where
    (d,s) = findDS $ n

-- | Tests numbers up to k that MR test is correct.
prop_millerRabinAll :: Int -> Bool
prop_millerRabinAll k = and $ map (\n -> millerRabinAll n == 
                                   primeDet n) [4..k]

-- | millerRabin k n does k MR rounds testing n for primality.
millerRabin :: (RandomGen g, Random a, Integral a) =>
  a -> a -> State g Bool
millerRabin k n = millerRabin' k
  where
    (d, s)          = findDS n
    millerRabin' 0 = return True
    millerRabin' k = do
      rest <- millerRabin' $ k - 1
      test <- randomR_st (1, n - 1)
      let this = millerRabinOnce n d s test
      return $ this && rest


-- | primeK k n. Probabilistic primality test of n
-- using k Miller-Rabin rounds.
primeK :: (Integral a, Random a, RandomGen g) => 
  a -> a -> State g Bool
primeK k n
  | n < 2            = return False
  | n == 2 || n == 3 = return True
  | otherwise        = millerRabin (min n k) n

-- | Probabilistic primality test with 64 Miller-Rabin rounds.
prime :: (Integral a, Random a, RandomGen g) => 
  a -> State g Bool
prime = primeK 64

prop_prime :: BigInt100000 -> Bool
prop_prime n = primeDet n == (eval $ prime n)


-- | List of all primes probabilistically determined.
primes :: (Integral a, Random a, RandomGen g) => State g [a]
primes = do
  let begin = filter primeDet [0..cutoff]
  end <- filterM prime [cutoff..]
  return $ begin ++ end
  where
    cutoff = 10^4