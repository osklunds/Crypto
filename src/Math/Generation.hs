
module Math.Generation
( nextCoprime
, genPrime
, genDifferentPrime
, nextPrime
)
where

import System.Random
import Control.Monad.State
import Test.QuickCheck

import Math.GCD
import Math.Prime
import Math.Random


-- | nextCoprime n s returns the next coprime to n starting 
-- from s.
nextCoprime :: Integral a => a -> a -> a
nextCoprime n s = head $ filter (coprime n) [s..]


-- | Generates b bit prime.
genPrime :: (RandomGen g, Random a, Integral a) => a -> State g a
genPrime b = do
  n  <- getRandomR (2^(b-1),2^b-1)
  ps <- filterM prime [n..]
  return $ head ps

-- | genDifferentPrime b p generates a b bit prime distinct 
-- from p.
genDifferentPrime :: (RandomGen g, Random a, Integral a) =>
  a -> a -> State g a
genDifferentPrime b p = do
  p' <- genPrime b
  if p == p'
    then genDifferentPrime b p
    else return p'


-- | nextPrime s returns the next prime starting from s.
nextPrime :: (RandomGen g, Random a, Integral a) =>
  a -> State g a
nextPrime s = filterM prime [s..] >>= return . head
