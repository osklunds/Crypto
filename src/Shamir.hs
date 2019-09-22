
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

import Math.Divisibility
import Math.BigInt
import Math.Gen
import Math.Prime
import Math.NumClass
import Tools

-- Couples a share si to the ID pi.
data ShareIdPair a = ShareIdPair a a
                   deriving (Eq)

instance Show a => Show (ShareIdPair a) where
  show (ShareIdPair s p) = "<share=" ++ 
                            show s ++ 
                            ",id=" ++ 
                            show p ++ 
                            ">"

-- Parameters n (number of parties), t (threshold) and q (modulo).
data ShamirParams a = ShamirParams a a a
                    deriving (Eq)

instance Show a => Show (ShamirParams a) where
  show (ShamirParams n t q) = "ShamirParams n=" ++ 
                              show n ++ 
                              " t=" ++ 
                              show t ++ 
                              " q=" ++ 
                              show q

validParams :: NumClass a => ShamirParams a -> Bool
validParams (ShamirParams n t q) = t >= 1
                                && n > t
                                && q >= 2
                                && prime q
                                && q > n
-- In the beta function, inverses of numbers up to n will be
-- calculated mod q. So if q is a prime > n all such inverses will exist.


share :: (NumClass a, RandomGen g) => 
  a -> ShamirParams a -> g -> ([ShareIdPair a], g)
share s p@(ShamirParams _ _ q) g
  | s /= s `mod` q      = error "s not in Zq"
  | not (validParams p) = error "Invalid parameters"
  | otherwise           = shareInner s p g

shareInner :: (NumClass a, RandomGen g) => 
  a -> ShamirParams a -> g -> ([ShareIdPair a], g)
shareInner s (ShamirParams n t q) g = (zipWith ShareIdPair sis pis, g'')
  where
    sis      = map polyFun pis
    polyFun  = poly (s:coeffs) q
    coeffs   = take t $ randomRs (0,q-1) g'
    (g',g'') = split g
    pis      = [1..n]

-- poly [c0..cm] q x computes the value of the polynomial
-- function with coefficients c0 to cm, at x, modulo q.
poly :: Integral a => [a] -> a -> a -> a
poly []     _ _ = 0
poly (c:cs) q x = (c + x*poly cs q x) `mod` q

prop_poly :: [BigInt7] -> BigInt7 -> BigInt7 -> Property
prop_poly cs q x = q >= 2 ==> sum1 == sum2
  where
    sum1  = poly cs q x
    sum2 = (sum $ zipWith (\c e -> c*x^e) cs [0..]) `mod` q


recover :: NumClass a => [ShareIdPair a] -> ShamirParams a -> a
recover pairs (ShamirParams n t q) = sum prods `mod` q
  where
    prods = zipWith (*) bi_s si_s
    bi_s  = map (beta pi_s q) pi_s
    pi_s  = map (\(ShareIdPair _ pi) -> pi) pairs
    si_s  = map (\(ShareIdPair si _) -> si) pairs

-- beta js q i calculates beta i, when parties js are 
-- involved, modulo q.
beta :: Integral a => [a] -> a -> a -> a
beta js q i = product [j * (j-i) `invMod` q | j <- js, j /= i] `mod` q

-- prop_beta g t te q, generates a degree t polynomial
-- and tests it with t+te beta values, modulo q.
prop_beta :: StdGen -> BigInt5 -> BigInt5 -> BigInt5 -> Property
prop_beta g t te q = t' >= 1 ==> p0 == p0'
  where
    (t', te', q')       = prop_beta_params t te q
    (xVals, coeffs, p0) = prop_beta_poly (t'+1+te') (t'+1) q' g
    
    yVals = map (poly coeffs q') xVals
    bVals = map (beta xVals  q') xVals
    -- polynomial at 0 using beta values
    p0'   = (sum $ zipWith (multMod q') bVals yVals) `mod` q'

prop_beta_params :: NumClass a => a -> a -> a -> (a, a, a)
prop_beta_params t te q = (t', te', q')
  where
    t'  = t  `mod` 40 -- Performance reasons
    te' = te `mod` 40 -- Performance reasons
    q'  = nextPrime (max q (t'+te'+2))
    -- The modulo must be a prime. And all x values must be coprime
    -- with the modulo, and we will grab x values from 1 to t'+1+te'
    -- which means we must have at least t'+te'+2 x values because
    -- they must be unique.

prop_beta_poly :: (NumClass a, RandomGen g) => 
  a -> a -> a -> g -> ([a], [a], a)
prop_beta_poly numX numC q g = (xVals, coeffs, p0)
  where
    (xVals, g') = randomRUs numX (1, q-1) g
    coeffs      = take numC $ randomRs (0, q-1) g'
    p0          = head coeffs

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

multMod :: Integral a => a -> a -> a -> a
multMod q a b = a*b `mod` q


prop_shareRecover :: StdGen -> 
                     BigInt5 -> 
                     ShamirParams BigInt5 ->
                     Bool
prop_shareRecover g s p@(ShamirParams n t q) = s_rec == s'
  where
    s' = s `mod` q -- Correctness
    (allPairs, g') = share s' p g

    (numParties, g'') = randomR (t+1, n) g'
    (pi_s, _)        = randomRUs numParties (1,n) g''

    pairs = [allPairs !! (pi-1) | pi <- pi_s]
    s_rec = recover pairs p

instance (NumClass a, Arbitrary a) => 
  Arbitrary (ShamirParams a) where
    arbitrary = do
      (n,t,q) <- arbitrary
      let n' = n `mod` 40 + 2           -- Performance
          t' = (t `mod` (n'-1)) + 1     -- Performance
          q' = nextPrime (max q n' + 1) -- Correctness
      return $ ShamirParams n' t' q'