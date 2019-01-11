
-- Module for finding primes and coprimes.

module Math.Gen
( nextCoprime
, nextPrime
, getPrime
, getDifferentPrime
)
where

import System.Random hiding (next)
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy hiding (next)
import Test.QuickCheck

import Math.GCD
import Math.Prime


next :: Enum a => (a -> Bool) -> a -> a
next c n = head $ filter c [n..]

-- nextCoprime n s returns the smallest coprime >= s to n.
nextCoprime :: Integral a => a -> a -> a
nextCoprime n = next (coprime n)

-- nextPrime n returns the smallest prime >= n.
nextPrime :: (Random a, Integral a) => a -> a
nextPrime = next prime


-- Generates b bit prime.
getPrime :: (Random a, Integral a, MonadRandom m) => a -> m a
getPrime b = do 
  n <- getRandomR (2^(b-1),2^b-1)
  return $ nextPrime n

-- getDifferentPrime b p generates a b bit prime distinct 
-- from p.
getDifferentPrime :: (Random a, Integral a, MonadRandom m) =>
  a -> a -> m a
getDifferentPrime b p = do
  p' <- getPrime b
  if p == p'
    then getDifferentPrime b p
    else return p'
