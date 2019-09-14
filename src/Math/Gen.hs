
module Math.Gen
( nextCoprime
, nextPrime
, randomPrime
, randomDiffPrime
)
where

import Test.QuickCheck
import System.Random
import Data.Bits

import Math.Divisibility
import Math.Prime


-- nextCoprime n s returns the smallest coprime, >= s, to n.
nextCoprime :: Integral a => a -> a -> a
nextCoprime n = nextMeetingCondition (coprime n)

nextMeetingCondition :: Enum a => (a -> Bool) -> a -> a
nextMeetingCondition c n = head $ filter c [n..]

nextPrime :: (Bits a, Random a, Integral a) => a -> a
nextPrime = nextMeetingCondition prime

-- Generates a random b bit prime.
randomPrime :: (RandomGen g, Random a, Integral a, Bits a) => a -> g -> (a, g)
randomPrime b g = (p, g)
  where
    (n, g') = randomR (2^(b-1),2^b-1) g
    p       = nextPrime n

-- Generates a random b bit prime different from p.
randomDiffPrime :: (RandomGen g, Random a, Integral a, Bits a) =>
  a -> a -> g -> (a, g)
randomDiffPrime b p g
  | p == p'   = randomDiffPrime b p g'
  | otherwise = result
  where
    result@(p', g') = randomPrime b g