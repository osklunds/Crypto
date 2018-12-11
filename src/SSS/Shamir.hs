
module SSS.Shamir
(
)
where

import System.Random

-- poly [c0..cm] x q computes the value of the polynomial
-- function with coefficients c0 to cm, at x, modulo q.
poly :: Integral a => [a] -> a -> a -> a
poly []     _ _ = 0
poly (c:cs) x q = c + x*poly cs x q `mod` q

-- Generates a list of n random integers modulo q.
randomList :: (RandomGen g, Random a, Integral a) => g -> a ->
                                                     a -> ([a], g)
randomList g 0 _ = ([],g)
randomList g n q = ((r:rest),g'')
  where
    (r,g')     = randomR (0,q) g
    (rest,g'') = randomList g' (n-1) q

-- share g s n t q computes the n secret shares of s, where
-- the threshold is t, modulo q.
share :: (RandomGen g, Random a, Integral a) => g -> a -> a -> a -> a -> ([a], g)
share g s n t q = (map p [1..n], g')
  where 
    (cs,g') = randomList g t q
    p i     = poly cs i q


