
module RSA
( PubKey
, PriKey
, randomKey
, encrypt
, decrypt
, sign
, verify
)
where

import Test.QuickCheck
import System.Random
import Data.Maybe

import Math.Gen
import Math.BigInt
import Math.Divisibility
import Math.PowerModulo
import Math.NumClass
import Tools

-- | PubKey n e.
data PubKey a = PubKey a a
-- | PriKey n d.
data PriKey a = PriKey a a

instance Show a => (Show (PubKey a)) where
  show (PubKey n e) = "Public key: N=" ++ 
                      show n ++ 
                      " e=" ++ 
                      show e

instance Show a => (Show (PriKey a)) where
  show (PriKey n d) = "Private key: N=" ++ 
                      show n ++ 
                      " d=" ++
                      show d


-- Generates a random b bit key pair.
randomKey :: (NumClass a, RandomGen g) => 
  a -> g -> ((PubKey a, PriKey a), g)
randomKey bits g
  | bits <= 4 = error "Too few bits"
  | otherwise = ((PubKey n e, PriKey n d), g')
    where
      ((n, phiN), g') = nAndPhiN bits g
      (e, d)          = eAndD phiN

nAndPhiN :: (NumClass a, RandomGen g) => a -> g -> ((a, a), g)
nAndPhiN bits g = ((n, phiN), g') 
  where
    ((p, q), g') = pAndQ bits g
    n            = p*q
    phiN         = (p-1)*(q-1)

pAndQ :: (NumClass a, RandomGen g) => a -> g -> ((a, a), g)
pAndQ bits g = ((p, q), g'')
  where
    (p, g')  = randomPrime bits g
    (q, g'') = randomDiffPrime bits p g'

eAndD :: NumClass a => a -> (a, a)
eAndD phiN = (e, d)
  where
      e = nextCoprime phiN 65537 `mod` phiN
      d = e `invMod` phiN


encrypt :: NumClass a => PubKey a -> a -> Maybe a
encrypt (PubKey n e) m
  | can n m   = Just $ powerModulo m e n
  | otherwise = Nothing

decrypt :: NumClass a => PriKey a -> a -> Maybe a
decrypt (PriKey n d) c
  | can n c   = Just $ powerModulo c d n
  | otherwise = Nothing

sign :: NumClass a => PriKey a -> a -> Maybe a
sign (PriKey n d) m
  | can n m   = Just $ powerModulo m d n
  | otherwise = Nothing

verify :: NumClass a => PubKey a -> a -> a -> Maybe Bool
verify (PubKey n e) m s
  | can n m   = Just $ m == powerModulo s e n
  | otherwise = Nothing

-- can n m determines if m can be encrypted/decrypted/
-- signed/verified using modulus n.
can :: NumClass a => a -> a -> Bool
can n m = m > 0 && m < n && coprime n m

prop_encryptThenDecrypt :: StdGen -> BigInt5 -> BigInt5 -> Property
prop_encryptThenDecrypt g b m = isJust c ==> Just m == m'
  where
    bits            = (5 + b `mod` 50)
    (pubKey,priKey) = fst $ randomKey bits g
    c               = encrypt pubKey m
    m'              = decrypt priKey (fromJust c)

prop_signThenVerify :: StdGen -> BigInt5 -> BigInt5 -> Property
prop_signThenVerify g b m = isJust s ==> v == Just True
  where
    bits            = (5 + b `mod` 50)
    (pubKey,priKey) = fst $ randomKey bits g
    s               = sign priKey m
    v               = verify pubKey m (fromJust s)