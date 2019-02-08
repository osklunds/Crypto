
-- | Additional functions for MonadRandom.

module Tools.MonadRandom
( getRandomRU
, getRandomRsU
)
where

import System.Random
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy
import Test.QuickCheck

-- | Random value not in the list and in range.
getRandomRU :: (Random a, Integral a, MonadRandom m) => 
  (a, a) -> [a] -> m a
getRandomRU range ns = do
  n <- getRandomR range
  if n `elem` ns
    then getRandomRU range ns
    else return n

-- | List of n unique random values in range.
getRandomRsU :: (Random a, Integral a, MonadRandom m) => 
  (a, a) -> a -> m [a]
getRandomRsU _     0 = return []
getRandomRsU range n = do
  us <- getRandomRsU range (n-1)
  u  <- getRandomRU  range us
  return (u:us)
