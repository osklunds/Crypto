
module Tools
( length
, take
, group
, toHex
, (!!)
, randomRUs
)
where

import Prelude hiding (length, take, drop, (!!))
import Numeric
import System.Random
import Test.QuickCheck

import Math.NumClass

length :: Integral a => [b] -> a
length []     = 0
length (a:as) = 1 + length as


take :: Integral a => a -> [b] -> [b]
take _ []     = []
take 0 _      = []
take n (a:as) = a:(take (n-1) as)


drop :: Integral a => a -> [b] -> [b]
drop _ []     = []
drop 0 as     = as
drop n (a:as) = drop (n-1) as


group :: Integral a => a -> [b] -> [[b]]
group _ [] = []
group n l  = (take n l) : (group n (drop n l))


toHex :: (Show a, Integral a) => a -> String
toHex n = showHex n ""


(!!) :: Integral a => [b] -> a -> b
[]     !! _ = error "Out of range"
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n-1)


randomRUs :: (NumClass a, RandomGen g) => a -> (a, a) -> g -> ([a], g)
randomRUs 0 _     g = ([], g)
randomRUs n range g = (this:rest, g'')
  where
    (rest, g')  = randomRUs (n-1) range g
    (this, g'') = randomRNotIn rest range g'

randomRNotIn :: (NumClass a, RandomGen g) => [a] -> (a, a) -> g -> (a, g)
randomRNotIn rs range g
  | r `elem` rs = randomRNotIn rs range g'
  | otherwise   = (r, g')
  where
    (r, g') = randomR range g
