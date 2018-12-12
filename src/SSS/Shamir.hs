
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

instance Arbitrary StdGen where
  arbitrary = do
    seed <- arbitrary
    return $ mkStdGen seed

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
share :: (RandomGen g, Random a, Integral a) => 
  g -> a -> a -> a -> a -> ([a], g)
share g s n t q
  | not (t >= 1 && n > t && q >= 2) = error "Invalid arguments"
  | otherwise                       = (map p [1..n], g')
  where 
    s' = s `mod` q
    -- cs are the list of coefficients
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

-- Generates a list of n unique random integers modulo q.
-- q should be much larger than n.
randomListU :: (RandomGen g, Random a, Integral a) => 
  g -> a -> a -> ([a], g)
randomListU g 0 _ = ([],g)
randomListU g n q = ((r:rest),g'')
  where
    (rest,g') = randomListU g (n-1) q
    (r,g'') = genNotIn g'

    --genNotIn :: g -> (a,g)
    genNotIn g1
      | r `elem` rest = genNotIn g2
      | otherwise     = (r,g2)
      where
        (r,g2) = randomR (0,q-1) g1

prop_beta :: StdGen -> BigInt1000 -> BigInt1000 -> Property
prop_beta g n' ne' = n >= 1 ==> s == s'
  where
    n          = n' `mod` 10
    ne         = ne' `mod` 20
    q          = findNextPrime 10000
    -- Coefficients to a polynom
    (cs@(s:_),g2) = randomList g n q
    -- Random x values, at least n of them
    (xVals,_)     = randomListU g2 (n+ne) q
    yVals      = map (\i -> poly cs i q) xVals
    -- Betas corresponding to the x values
    betas      = map (\i -> beta i xVals q) xVals
    -- Sum using beta values
    s'         = (sum $ zipWith (*) betas yVals) `mod` q

-- oneTest g s n t q: share the secret s to n parties with
-- threshold t. modulo q. Tests if can recover the secret.
oneTest :: (RandomGen g, Random a, Integral a) => g -> a -> a -> a -> a -> Bool
oneTest g s n t q = recover [1..n] sis q == (s `mod` q)
  where
    (sis,_) = share g s n t q

prop_shareRecover :: StdGen -> 
                     BigInt100000 -> 
                     BigInt100000 -> 
                     BigInt100000 -> 
                     BigInt100000 -> 
                     Property
prop_shareRecover g s' n' t' q' = n > 0 && t >= 1 && n > t ==>
  oneTest g s n t q
  where
    q = findNextPrime q'
    s = s' `mod` q
    n = (n' `mod` q) `mod` 30
    t = (t' `mod` n)

    (sis, g2) = share g s n t q
    (pis, _)  = randomList g2 (t+1) q