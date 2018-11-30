
module Math.Generation
( genPrime
, findNextCoprime
)
where

import System.Random
import Test.QuickCheck

import Math.GCD
import Math.Prime

-- Find next integer satisfying criteria c
-- Unless c can be satisfied, it will go on forever
findNext :: Integral a => (a -> Bool) -> a -> a
findNext c n
  | c n       = n
  | otherwise = findNext c $ n+1

-- Generates a prime with specified number of bits
genPrime :: (Random a, Integral a) => a -> Gen a
genPrime bits = do
  n <- choose (2^(bits-1),2^bits-1)
  let p = findNext prime n
  return p

-- Finds the next coprime to n, starting from s
findNextCoprime :: Integral a => a -> a -> a
findNextCoprime n s = findNext (coprime n) s