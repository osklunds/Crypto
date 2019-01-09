
-- Module for primality testing and list of primes.
-- Both probabilistic (fast but cumbersome) and
-- deterministic testing is provided.

module Math.Prime
( 
  prime
)
where

import System.Random
import Control.Monad
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy
import Test.QuickCheck
import Prelude hiding (take)

import Math.Common
import Math.PowerModulo
import Math.BigInt
import Tools


primeDet :: Integral a => a -> Bool
primeDet n
  | n < 2  = False
  | n >= 2 = null . filter (`divides` n) $ [2..n-1]


-- Finds (d, s) s.t. n - 1 = d*2^s, d odd.
findDS :: Integral a => a -> (a, a)
findDS n = head [(d, s) | s <- [0..], 
                        let d = (n-1) `div` (2^s), 
                        n-1 == d*2^s,
                        odd d]

-- millerRabinOnce n a does one MR round on n using a.
millerRabinOnce :: Integral a => a -> a -> Bool
millerRabinOnce n a
  | even n           = False
  | otherwise        = not (test1 && test2)
  where
    (d, s) = findDS n

    test1 = powerModulo a d n /= 1
    test2 = and $ map (\t -> powerModulo a ((2^t)*d) n /= n-1) 
                      [0..s-1]

-- Does MR test on n with all test values a for n.
millerRabinAll :: Integral a => a -> Bool
millerRabinAll n
  | n <= 3    = error "Too small for Miller-Rabin"
  | otherwise = and $ map (millerRabinOnce n) [1..n-1]

prop_millerRabinAll :: Int -> Bool
prop_millerRabinAll k = and $ map (\n -> millerRabinAll n == 
                                   primeDet n) [4..k]

-- millerRabin k n does k MR rounds testing n for primality.
millerRabin :: (Random a, Integral a, MonadRandom m) =>
  a -> a -> m Bool
millerRabin k n = do
  as <- getRandomRs (1, n - 1)
  return . and . take k $ map (millerRabinOnce n) as


primeK :: (Integral a, Random a, MonadRandom m) => 
  a -> a -> m Bool
primeK k n
  | n < 2            = return False
  | n == 2 || n == 3 = return True
  | otherwise        = millerRabin (min n k) n

prime :: (Integral a, Random a, MonadRandom m) => 
  a -> m Bool
prime = primeK 64

prop_prime :: BigInt100000 -> Bool
prop_prime n = let res = fst $ runRand (prime n) $ mkStdGen 123
               in  res == primeDet n
