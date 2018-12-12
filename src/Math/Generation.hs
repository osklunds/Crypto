
module Math.Generation
( genPrime
, genDifferentPrime
, findNextPrime
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

-- Generates a prime with b bits
genPrime :: (RandomGen g, Random a, Integral a) => g -> a -> (a,g)
genPrime g b = (p,g')
  where
    (n,g') = randomR (2^(b-1),2^b-1) g
    p      = findNext prime n

-- Same as genPrime, but a prime different to p suppliad
genDifferentPrime :: (RandomGen g, Random a, Integral a) => g -> 
                     a -> a -> (a,g)
genDifferentPrime g b p
  | p == p'   = genDifferentPrime g' b p
  | otherwise = (p',g')
  where
    (p',g') = genPrime g b

-- Finds the next prime, starting from the argument
findNextPrime :: (Random a, Integral a) => a -> a
findNextPrime = findNext prime

-- Finds the next coprime to n, starting from s
findNextCoprime :: Integral a => a -> a -> a
findNextCoprime n s = findNext (coprime n) s