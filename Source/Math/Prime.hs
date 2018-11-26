
module Math.Prime
(
)
where

import System.Random

import Math.GCD
import Math.PowerModulo

primeNaive :: Integral a => a -> Bool
primeNaive n
  | n < 2  = False
  | n >= 2 = null [d | d <- [2..n-1], d `divides` n]


-- Does one MR round for an a
millerRabinOnce :: Integral a => a -> a -> a -> a -> Bool
millerRabinOnce n a d s
  | even n           = False
  | otherwise        = not (test1 && test2)
  where
    (d,s) = findDS n

    test1 = powerModulo a d n /= 1
    test2 = and $ map (\t -> powerModulo a ((2^t)*d) n /= n-1) 
                      [0..s-1]

-- Finds d and s for n
findDS :: Integral a => a -> (a, a)
findDS n = findDS' (n-1) 0
  where
    findDS' q s
      | even q = findDS' (q `div` 2) (s+1)
      | odd  q = (q,s)

-- Tests MR for all a
millerRabinAll :: Integral a => a -> Bool
millerRabinAll n
  | n <= 3    = error "Too small for MR"
  | otherwise = and $ map (\a -> millerRabinOnce n a d s) [1..n-1]
  where
    (d,s) = findDS $ n

test_millerRabinAll :: Integral a => a -> Bool
test_millerRabinAll k = and $ map (\n -> millerRabinAll n == 
                                 primeNaive n) [4..k]

millerRabin :: (RandomGen g, Random a, Integral a) => a -> a -> 
                                                 g -> (Bool, g)
millerRabin n k gen = millerRabin' n k gen
  where
    (d,s)    = findDS n

    millerRabin' _ 0 gen = (True, gen)
    millerRabin' n k gen
      | cand      = millerRabin' n (k-1) gen'
      | otherwise = (False, gen')
      where
        (a,gen')  = randomR (1, n-1) gen
        cand      = millerRabinOnce n a d s

prime :: (Integral a, Random a) => a -> Bool
prime n
  | n < 2            = False
  | n == 2 || n == 3 = True
  | otherwise        = fst $ millerRabin n k (mkStdGen 1337)
  where
    k = min n 1000