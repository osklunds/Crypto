
module SSS.Shamir
( share
, recover
)
where

import System.Random
import Test.QuickCheck

import Math.GCD
import Math.BigInt
import Math.Generation


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


-- Generates a list of n random integers modulo q.
randomList :: (RandomGen g, Random a, Integral a) => 
  g -> a -> a -> ([a], g)
randomList g 0 _ = ([],g)
randomList g n q = ((r:rest),g'')
  where
    (r,g')     = randomR (0,q-1) g
    (rest,g'') = randomList g' (n-1) q

instance Arbitrary StdGen where
  arbitrary = do
    seed <- arbitrary
    return $ mkStdGen seed

prop_randomList :: StdGen -> BigInt1000 -> 
                   BigInt1000 -> Property
prop_randomList g n q = n > 0 && q > 2 ==> 
  length rl == fromIntegral n && 
  null (filter (<0) rl) && 
  null (filter (>=q) rl)
  where
    (rl,_) = randomList g n q


-- share g s n t q computes the n secret shares of s, where
-- the threshold is t, modulo q.
share :: (RandomGen g, Random a, Integral a) => 
  g -> a -> a -> a -> a -> ([a], g)
share g s n t q
  | not (t >= 1 && n > t && q >= 2) = error "Invalid arguments"
  | n >= q                           = 
    error "Modulo must be greater than number of shares"
  | otherwise                       = (map polyFun [1..n], g')
  where 
    -- Generate random coefficients for the polynomial
    (cs,g') = randomList g t q
    polyFun = poly (s:cs) q


-- beta js q i calculates beta i, when parties js are 
-- involved, modulo q.
beta :: Integral a => [a] -> a -> a -> a
beta []     _ _ = 1
beta (j:js) q i
  | i == j    = beta js q i
  | otherwise = j * ((j-i) `invMod` q) * beta js q i `mod` q

-- Generates a list of n unique random integers modulo q.
-- q should be much larger than n to speed up calculation.
randomListU :: (RandomGen g, Random a, Integral a) => 
  g -> a -> a -> ([a], g)
randomListU g 0 _ = ([],g)
randomListU g n q = ((r:rest),g'')
  where
    (rest,g') = randomListU g (n-1) q
    (r,g'') = randomNotIn g'

    randomNotIn g1
      | r `elem` rest = randomNotIn g2
      | otherwise     = (r,g2)
      where
        (r,g2) = randomR (0,q-1) g1

-- prop_beta g n ne q, generates a degree n polynomial
-- and tests it with n+ne beta values, modulo q.
prop_beta :: StdGen -> 
             BigInt100000 -> 
             BigInt100000 -> 
             BigInt100000 -> 
             Property
prop_beta g n ne q = n' >= 1 ==> s == s'
  where
    -- Need to make the modulo large enough so that
    -- is a prime greater than largest j in the
    -- beta calculation
    q'  = findNextPrime (max q (n'+ne'+1))
    n'  = n `mod` 40  -- Cap the degree
    ne' = ne `mod` 20 -- Cap the number of extra points

    -- Coefficients to the polynom
    (cs@(s:_),g1) = randomList g n' q'

    -- n'+ne' unique random x values
    (xVals,_) = randomListU g1 (n'+ne') q'
    -- Corresponding y values of the polynomial
    yVals     = map (poly cs q')    xVals
    -- Betas corresponding to the x values
    bVals     = map (beta xVals q') xVals

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
                     BigInt10000000 -> 
                     BigInt10000000 -> 
                     BigInt10000000 -> 
                     BigInt10000000 -> 
                     Property
prop_shareRecover g s n t q = n' > 0 && t' >= 1 && n' > t' ==>
  s_rec == s_rec
  where
    n' = n `mod` 60 + 2
    t' = (t `mod` n') + 1
    q' = findNextPrime (max q n'+1)
    s' = s `mod` q'

    (si_sAll, g2) = share g s' n' t' q'
    -- Picks t'+1 parties that will recover the secret
    (pi_s, _)  = randomListU g2 (t'+1) n'
    pi_s'      = map (+1) pi_s
    si_s       = map (\pi -> si_sAll !! (fromIntegral pi - 1)) pi_s'

    s_rec = recover pi_s' si_s q'
