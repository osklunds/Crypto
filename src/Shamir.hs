
-- | Shamir's Secret Sharing.

module Shamir
( ShareIdPair(..)
, ShamirParams(..)
, validParams
, share
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


-- | Couples a share si to the ID pi.
data ShareIdPair a = ShareIdPair a a
                   deriving (Eq)

instance Show a => Show (ShareIdPair a) where
  show (ShareIdPair s p) = "<share=" ++ 
                            show s ++ 
                            ",id=" ++ 
                            show p ++ 
                            ">"

-- | Parameters n (number of parties), t (threshold) and q (modulo).
data ShamirParams a = ShamirParams a a a
                    deriving (Eq)

instance Show a => Show (ShamirParams a) where
  show (ShamirParams n t q) = "ShamirParams n=" ++ 
                              show n ++ 
                              " t=" ++ 
                              show t ++ 
                              " q=" ++ 
                              show q

-- In the beta function, inverses of numbers
-- at most n will be calculated mod q. So if q is prime > n
-- such inverses will exist.
validParams :: (Integral a, Random a) => 
  ShamirParams a -> Bool
validParams (ShamirParams n t q) = t >= 1
                                && n > t
                                && q >= 2
                                && prime q
                                && n < q

share :: (Integral a, Random a, MonadRandom m) => 
  a -> ShamirParams a -> m [ShareIdPair a]
share s p@(ShamirParams n t q)
  | s /= s `mod` q      = error "s not in Zq"
  | not (validParams p) = error "Invalid parameters"
  | otherwise = do
      infCoeffs   <- getRandomRs (0,q-1)
      let coeffs  =  s:(take t infCoeffs)
          polyFun =  poly coeffs q
      return [ShareIdPair (polyFun pi) pi | pi <- [1..n]]


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


recover :: Integral a => [ShareIdPair a] -> ShamirParams a -> a
recover pairs (ShamirParams n t q) = sum prods `mod` q
  where
    si_s  = map (\(ShareIdPair si _) -> si) pairs
    pi_s  = map (\(ShareIdPair _ pi) -> pi) pairs
    bi_s  = map (beta pi_s q) pi_s
    prods = zipWith (*) bi_s si_s


instance (Integral a, Random a, Arbitrary a) => 
  Arbitrary (ShamirParams a) where
    arbitrary = do
      (n,t,q) <- arbitrary
      let n' = n `mod` 40 + 2           -- Performance
          t' = (t `mod` (n'-1)) + 1     -- Performance
          q' = nextPrime (max q n' + 1) -- Correctness
      return $ ShamirParams n' t' q'

prop_shareRecover :: StdGen -> 
                     BigInt100000 -> 
                     ShamirParams BigInt100000 ->
                     Bool
prop_shareRecover g s p@(ShamirParams n t q) = s_rec == s'
  where
    s' = s `mod` q -- Correctness
    
    (allPairs, pi_s) = fst $ runRand f g
      where
        f = do
          allPairs   <- share s' p
          numParties <- getRandomR   (t+1,n)
          pi_s       <- getRandomRsU (1,n) numParties
          return (allPairs, pi_s)

    pairs = [allPairs !! (pi-1) | pi <- pi_s]
    s_rec = recover pairs p
