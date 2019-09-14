
module Math.Divisibility
( divides
, gcd
, coprime
, eea
, invMod
)
where

import Prelude hiding (gcd)
import qualified Prelude (gcd)
import Test.QuickCheck
import Math.BigInt

-- divides d n tests if d divides n.
divides :: Integral a => a -> a -> Bool
d `divides` n = n `mod` d == 0

prop_divides :: BigInt7 -> BigInt7 -> Property
prop_divides d f = d /= 0 ==> d `divides` (d*f)


gcd :: Integral a => a -> a -> a
gcd a b
  | a < 0 || b < 0 = gcd (abs a) (abs b)
  | b == 0         = a
  | otherwise      = gcd b (a `mod` b)

prop_gcd :: BigInt7 -> BigInt7 -> Property
prop_gcd a b = collect (gcd a b) $ gcd a b == Prelude.gcd a b


coprime :: Integral a => a -> a -> Bool
a `coprime` b = gcd a b == 1


-- eea a b returns (d,s,t) s.t. d=as+bt. d>0 unless a=b=0.
eea :: Integral a => a -> a -> (a, a, a)
eea a b
  | b == 0         = (abs a,signum a,0)
  | a < 0 || b < 0 = let (d,s,t) = eea (abs a) (abs b)
                     in  (d, signum a * s, signum b * t)
  
  | otherwise      = (d,s,t)
  where
    r         = a `mod` b
    q         = a `div` b
    (d,s',t') = eea b r
    (s,t)     = (t',s'-q*t')

prop_eea :: BigInt7 -> BigInt7 -> Bool
prop_eea a b = gcdCorrect && gcdPositive && prodSumCorrect
  where
    (d,s,t)        = eea a b
    gcdCorrect     = gcd a b == d
    gcdPositive    = if a /= 0 || b /= 0 then d > 0 else d == 0
    prodSumCorrect = a*s+b*t == d


-- a `invMod` m returns the inverse of a modulo m. 
-- Requirements: a>0, m>=2, a and m coprime.
invMod :: Integral a => a -> a -> a
a `invMod` m
  | d /= 1    = error "Not coprime"
  | otherwise = s `mod` m
  where
    (d,s,_) = eea a m

prop_invMod :: BigInt7 -> BigInt7 -> Property
prop_invMod a m = a > 0 && m >= 2 && a `coprime` m ==>
  aInv > 0 && aInv < m && a*aInv `mod` m == 1
  where
    aInv = a `invMod` m
