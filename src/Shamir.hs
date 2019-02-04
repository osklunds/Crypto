
module Shamir
( share
, recover
)
where

import Prelude hiding ((!!), length, take)
import System.Random
import Test.QuickCheck
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy

import Math.Divisibility
import Math.BigInt
import Math.Gen
import Tools

-- poly [c0..cm] q x computes the value of the polynomial
-- function with coefficients c0 to cm, at x, modulo q.
poly :: Integral a => [a] -> a -> a -> a
poly []     _ _ = 0
poly (c:cs) q x = (c + x*poly cs q x) `mod` q

prop_poly :: [BigInt10000000] -> 
             BigInt10000000 ->                              
             BigInt10000000 -> 
             Property
prop_poly cs q x = q >= 2 ==> sum1 == sum2
  where
    sum1  = poly cs q x
    sum2 = (sum $ zipWith (\c e -> c*x^e) cs [0..]) `mod` q


-- share g s n t q computes the n secret shares of s, where
-- the threshold is t, modulo q.
share :: (Random a, Integral a, MonadRandom m) => 
  a -> a -> a -> a -> m [a]
share s n t q
  | not (t >= 1 && 
         n > t && 
         q >= 2) &&
         n >= q = error "Invalid arguments"
  | otherwise    = do
    infCoeffs <- getRandomRs (0,q-1)
    let coeffs  = s:(take t infCoeffs)
        polyFun = poly coeffs q
    return $ map polyFun [1..n]


-- beta js q i calculates beta i, when parties js are 
-- involved, modulo q.
beta :: Integral a => [a] -> a -> a -> a
beta []     _ _ = 1
beta (j:js) q i
  | i == j    = beta js q i
  | otherwise = j * ((j-i) `invMod` q) * beta js q i `mod` q

-- Generates a list of n unique random integers modulo q.
-- q should be much larger than n to speed up calculation.
getRandomListU :: (Random a, Integral a, MonadRandom m) => 
  a -> a -> m [a]
getRandomListU 0 _ = return []
getRandomListU n q = do
  rs <- getRandomListU (n-1) q
  r <- getRandomNotIn rs q
  return (r:rs)

getRandomNotIn :: (Random a, Integral a, MonadRandom m) => 
  [a] -> a -> m a
getRandomNotIn ns q = do
  n <- getRandomR (0,q-1)
  if n `elem` ns
    then getRandomNotIn ns q
    else return n

-- prop_beta g n ne q, generates a degree n polynomial
-- and tests it with n+ne beta values, modulo q.
prop_beta :: StdGen -> 
             BigInt100000 -> 
             BigInt100000 -> 
             BigInt100000 -> 
             Property
prop_beta g n ne q = n' >= 1 ==> s == s'
  where
    n'  = n `mod` 40  -- Cap the degree
    ne' = ne `mod` 20 -- Cap the number of extra points

    -- Need to make the modulo large enough so that
    -- is a prime greater than largest j in the
    -- beta calculation
    q' = nextPrime (max q (n'+ne'+1))

    -- cs: n' coefficients to the polynomial
    -- xVals: n'+ne' unique random x values
    (cs, xVals) = fst $ runRand f g
    s = head cs
    f = do
      cs <- getRandomRs (0,q'-1)
      xVals <- getRandomListU (n'+ne') q'
      return (take n' cs, take (n'+ne') xVals)
    
    yVals = map (poly cs    q') xVals
    bVals = map (beta xVals q') xVals

    -- Sum using beta values
    s' = (sum $ zipWith (*) bVals yVals) `mod` q'


-- recover pi_s si_s q, recovers the share using the
-- shares si_s from parties pi_s, modulo q.
recover :: Integral a => [a] -> [a] -> a -> a
recover pi_s si_s q = sum prods `mod` q
  where
    bi_s  = map (beta pi_s q) pi_s
    prods = zipWith (*) bi_s si_s

prop_shareRecover :: StdGen -> 
                     BigInt100000 -> 
                     BigInt100000 -> 
                     BigInt100000 -> 
                     BigInt100000 -> 
                     Property
prop_shareRecover g s n t q = n' > 0 && t' >= 1 && n' > t' ==>
  s_rec == s'
  where
    n' = n `mod` 40 + 2
    t' = (t `mod` (n'-1)) + 1
    q' = nextPrime (max q n'+1)
    s' = s `mod` q'
    
    (si_sAll, pi_s) = fst $ runRand f g
    f = do
      si_sAll <- share s' n' t' q'
      -- Picks between t'+1 and n' parties that collaborate
      numP    <- getRandomR (t'+1,n')
      pi_s    <- getRandomListU numP n'
      return (si_sAll, pi_s)

    pi_s'      = map (+1) pi_s
    si_s       = [si_sAll !! pi | pi <- pi_s]

    s_rec = recover pi_s' si_s q'
