
module Math.Gen
( nextCoprime
, nextPrime
, getPrime
, getDiffPrime
)
where

import System.Random hiding (next)
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy hiding (next)
import Test.QuickCheck

import Math.Divisibility
import Math.Prime


next :: Enum a => (a -> Bool) -> a -> a
next c n = head $ filter c [n..]

-- | nextCoprime n s returns smallest coprime, >= s, to n.
nextCoprime :: Integral a => a -> a -> a
nextCoprime n = next (coprime n)

nextPrime :: (Random a, Integral a) => a -> a
nextPrime = next prime


-- | Generates b bit prime.
getPrime :: (Random a, Integral a, MonadRandom m) => a -> m a
getPrime b = do 
  n <- getRandomR (2^(b-1),2^b-1)
  return $ nextPrime n

-- | getDiffPrime b p generates a b bit prime distinct 
-- from p.
getDiffPrime :: (Random a, Integral a, MonadRandom m) =>
  a -> a -> m a
getDiffPrime b p = do
  p' <- getPrime b
  if p == p'
    then getDiffPrime b p
    else return p'
