
-- Module for finding primes and coprimes.

module Math.Gen
( nextCoprime
, nextPrime
, genPrime
, genDifferentPrime
)
where

import System.Random
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy
import Test.QuickCheck

import Math.GCD
import Math.Prime


-- nextCoprime n s returns the smallest coprime >= s to n.
nextCoprime :: Integral a => a -> a -> a
nextCoprime n s = head $ filter (coprime n) [s..]

-- nextPrime n returns the smallest prime >= n.
nextPrime :: (Random a, Integral a, MonadRandom m) =>
  a -> m a
nextPrime s = filterM prime [s..] >>= return . head


-- Generates b bit prime.
genPrime :: (Random a, Integral a, MonadRandom m) => a -> m a
genPrime b = do
  n  <- getRandomR (2^(b-1),2^b-1)
  ps <- filterM prime [n..]
  return $ head ps

-- genDifferentPrime b p generates a b bit prime distinct 
-- from p.
genDifferentPrime :: (Random a, Integral a, MonadRandom m) =>
  a -> a -> m a
genDifferentPrime b p = do
  p' <- genPrime b
  if p == p'
    then genDifferentPrime b p
    else return p'
