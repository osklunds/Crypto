
module Math.GCD
( divides
, gcd
, eea
, coprime
, invMod
)
where

import Prelude hiding (gcd)
import Test.QuickCheck

import Math.BigInt

divides :: Integral a => a -> a -> Bool
a `divides` b = b `mod` a == 0

gcd :: Integral a => a -> a -> a
gcd a b
  | a < 0 || b < 0 = gcd (abs a) (abs b)
  | b > a          = gcd b a
  | b == 0         = a
  | b `divides` a  = b
  | otherwise      = gcd b (a `mod` b)

-- Given a and b, returns (d,s,t) s.t. gcd(a,b)=d=as+bt
eea :: Integral a => a -> a -> (a, a, a)
eea a b
  -- Special cases
  | a == 0 && b == 0 = (0,0,0)
  | a == 0           = (abs b,0,signum b)
  | b == 0           = (abs a,signum a,0)
  | a < 0 && b > 0   = let (d,s,t) = eea (-a) b
                       in  (d,-s,t)
  | a > 0 && b < 0   = let (d,s,t) = eea a (-b)
                       in  (d,s,-t)
  | a < 0 && b < 0   = let (d,s,t) = eea (-a) (-b)
                       in  (d,-s,-t)

  -- Regular/recursive cases
  | a == b = (a,1,0)
  | r == 0 = (b,0,1)
  | r /= 0 = (d,s,t)
  where
    r = a `mod` b
    q = a `div` b
    (d,s',t') = eea b r
    (s,t) = (t',s'-q*t')

prop_eea :: BigInt10000000 -> BigInt10000000 -> Bool
prop_eea a b = gcd a b == d && a*s+b*t == d && d >= 0
  where
    (d,s,t) = eea a b

-- Multiplicative inverse of a mod m
invMod :: Integral a => a -> a -> a
a `invMod` m
  | d /= 1    = error "Not coprime"
  | otherwise = s `mod` m
  where
    (d,s,_) = eea a m

coprime :: Integral a => a -> a -> Bool
a `coprime` b = gcd a b == 1

prop_invMod :: BigInt10000000 -> BigInt10000000 -> Property
prop_invMod a m = a > 0 && m > 2 && a `coprime` m ==>
  i > 0 && i < m && a*i `mod` m == 1
  where
    i = a `invMod` m
