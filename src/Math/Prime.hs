
-- Module for primality testing.

module Math.Prime
( prime
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

-- Does MR test on n with all test values, i.e. [1..n-1].
millerRabinAll :: Integral a => a -> Bool
millerRabinAll n
  | n <= 3    = error "Too small for Miller-Rabin"
  | otherwise = and $ map (millerRabinOnce n) [1..n-1]

prop_millerRabinAll :: Int -> Bool
prop_millerRabinAll k = and $ map (\n -> millerRabinAll n == 
                                   primeDet n) [4..k]

-- millerRabin n k does k MR rounds testing n for primality.
millerRabin :: (Random a, Integral a, MonadRandom m) =>
  a -> a -> m Bool
millerRabin n k = do
  as <- getRandomRs (1, n - 1)
  return . and . take k $ map (millerRabinOnce n) as

-- The final prime function. To provide a simpler interface,
-- the randomness is hard-coded.
prime :: (Integral a, Random a) => a -> Bool
prime n
  | n <  10000 = primeDet n
  | n >= 10000 = fst $ runRand (millerRabin n 64) $ mkStdGen 123
  -- A cutoff is used. Below, faster with naive. Above,
  -- faster with Miller-Rabin.

prop_prime :: BigInt100000 -> Bool
prop_prime n = prime n == primeDet n
