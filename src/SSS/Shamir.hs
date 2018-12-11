
-- NOTE: not working yet

module SSS.Shamir
(
)
where

import System.Random
import Test.QuickCheck

import Math.GCD
import Math.BigInt
import Math.Generation

-- poly [c0..cm] x q computes the value of the polynomial
-- function with coefficients c0 to cm, at x, modulo q.
poly :: Integral a => [a] -> a -> a -> a
poly []     _ _ = 0
poly (c:cs) x q = (c + x*poly cs x q `mod` q) `mod` q

prop_poly :: [BigInt10000000] -> BigInt10000000 ->                               BigInt10000000 -> 
                                 Property
prop_poly cs x q = q > 2 ==> sum1 == sum2
  where
    sum1  = poly cs x q
    sum2 = (sum $ zipWith (\c e -> c*x^e) cs [0..]) `mod` q

-- Generates a list of n random integers modulo q.
randomList :: (RandomGen g, Random a, Integral a) => g -> a ->
                                                     a -> ([a], g)
randomList g 0 _ = ([],g)
randomList g n q = ((r:rest),g'')
  where
    (r,g')     = randomR (0,q-1) g
    (rest,g'') = randomList g' (n-1) q

prop_randomList :: Int -> BigInt1000 -> BigInt1000 -> Property
prop_randomList g n q = n > 0 && q > 2 ==> 
  length rl == fromIntegral n && 
  null (filter (<0) rl) && 
  null (filter (>=q) rl)
  where
    (rl,_) = randomList (mkStdGen g) n q

-- share g s n t q computes the n secret shares of s, where
-- the threshold is t, modulo q.
share :: (RandomGen g, Random a, Integral a) => g -> a -> a -> a -> a -> ([a], g)
share g s n t q = (map p [1..n], g')
  where 
    (cs,g') = randomList g t q
    p i     = poly (s:cs) i q

-- recover pis sis q, recovers the share using the
-- shares sis from parties pis, modulo q.
recover :: Integral a => [a] -> [a] -> a -> a
recover pis sis q = sum prods `mod` q
  where
    bis   = map (\pi -> beta pi pis q) pis
    prods = zipWith (*) bis sis

-- beta i js q calculates beta i, when parties js are involved,
-- modulo q.
beta :: Integral a => a -> [a] -> a -> a
beta _ []     _ = 1
beta i (j:js) q
  | i == j    = beta i js q
  | otherwise = j * ((j-i) `invMod` q) * beta i js q `mod` q

oneTest :: (RandomGen g, Random a, Integral a) => g -> a -> a -> a -> (Bool,g)
oneTest g s n t = (recover [1..n] sis 1009 == s, g')
  where
    (sis,g') = share g s n t 1009

prop_shareRecover :: Int -> BigInt1000 -> BigInt1000 -> BigInt1000 -> BigInt1000 -> Property
prop_shareRecover g' s' n' t' q' = t > 0 && n > t ==> recover pis sis q == s
  where
    g = mkStdGen g'
    q = findNextPrime q'
    s = s' `mod` q
    n = (n' `mod` q) `mod` 10
    t = (t' `mod` q) `mod` 3

    (sis, g2) = share g s n t q
    (pis, _)  = randomList g2 (t+1) q