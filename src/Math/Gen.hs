
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
import Math.NumClass


-- nextCoprime n s returns the smallest coprime, >= s, to n.
nextCoprime :: NumClass a => a -> a -> a
nextCoprime n = nextMeetingCondition (coprime n)

nextMeetingCondition :: Enum a => (a -> Bool) -> a -> a
nextMeetingCondition c n = head $ filter c [n..]

nextPrime :: NumClass a => a -> a
nextPrime = nextMeetingCondition prime

-- Generates a random b bit prime.
randomPrime :: (NumClass a, RandomGen g) => a -> g -> (a, g)
randomPrime b g = (p, g)
  where
    (n, g') = randomR (2^(b-1),2^b-1) g
    p       = nextPrime n

-- Generates a random b bit prime different from p.
randomDiffPrime :: (NumClass a, RandomGen g) =>
  a -> a -> g -> (a, g)
randomDiffPrime b p g
  | p == p'   = randomDiffPrime b p g'
  | otherwise = result
  where
    result@(p', g') = randomPrime b g