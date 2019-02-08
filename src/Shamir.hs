
-- | Shamir's Secret Sharing.

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
import Math.Prime
import Tools
import Tools.MonadRandom

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


-- | share s n t q computes the n secret shares of s, where
-- the threshold is t, modulo q.
share :: (Integral a, Random a, MonadRandom m) => 
  a -> a -> a -> a -> m [a]
share s n t q
  | not (t >= 1 && n > t && q >= 2) = error "Invalid arguments"
  | not (prime q) || n >= q         = error "Bad modulus"
  -- The reason is, in beta function, inverses of numbers
  -- at most n will be calculated mod q. So if q is prime > n
  -- such inverses will exist.
  | otherwise = do
      infCoeffs   <- getRandomRs (0,q-1)
      let coeffs  =  s:(take t infCoeffs)
          polyFun =  poly coeffs q
      return $ map polyFun [1..n]


-- beta js q i calculates beta i, when parties js are 
-- involved, modulo q.
beta :: Integral a => [a] -> a -> a -> a
beta js q i = product [j * (j-i) `invMod` q | j <- js, j /= i] `mod` q

-- prop_beta g n ne q, generates a degree n polynomial
-- and tests it with n+ne beta values, modulo q.
prop_beta :: StdGen -> 
             BigInt100000 -> 
             BigInt100000 -> 
             BigInt100000 -> 
             Property
prop_beta g n ne q = n' >= 1 ==> s == s'
  where
    n'    = n  `mod` 40 -- Performance reason
    ne'   = ne `mod` 20 -- Performance reason
    q'    = nextPrime (max q (n'+ne'+1))
    range = (0,q'-1)

    -- cs:    n' coefficients to the polynomial
    -- xVals: n'+ne' unique random x values
    (cs, xVals) = fst $ runRand f g
      where
        f = do
          cs    <- getRandomRs  (0,q'-1)
          xVals <- getRandomRsU (0,q'-1) (n'+ne')
          return (take n' cs, xVals)
    s = head cs
    
    yVals = map (poly cs    q') xVals
    bVals = map (beta xVals q') xVals

    -- polynomial at 0 using beta values
    s' = (sum $ zipWith (*) bVals yVals) `mod` q'


-- | recover si_s pi_s q, recovers the share using the
-- shares si_s from parties pi_s, modulo q.
recover :: Integral a => [a] -> [a] -> a -> a
recover si_s pi_s q = sum prods `mod` q
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
    n' = n `mod` 40 + 2         -- Performance
    t' = (t `mod` (n'-1)) + 1   -- Performance
    q' = nextPrime (max q n'+1) -- Correct arguments
    s' = s `mod` q'             -- Performance
    
    (si_sAll, pi_s) = fst $ runRand f g
      where
        f = do
          si_sAll <- share s' n' t' q'
          -- Picks between t'+1 and n' parties that collaborate
          numP    <- getRandomR   (t'+1,n')
          pi_s    <- getRandomRsU (1,n') numP
          return (si_sAll, pi_s)

    si_s  = [si_sAll !! (pi-1) | pi <- pi_s]
    s_rec = recover si_s pi_s q'
