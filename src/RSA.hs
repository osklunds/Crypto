
module RSA
( genKey
, PubKey
, PriKey
, canEncrypt
, canDecrypt
, canSign
, canVerify
, encrypt
, decrypt
, sign
, verify
)
where

import System.Random
import Test.QuickCheck

import Math.Generation
import Math.BigInt
import Math.GCD
import Math.PowerModulo

-- Public key (N,e)
data PubKey a = PubKey a a
-- Private key (N,d)
data PriKey a = PriKey a a

instance Show a => (Show (PubKey a)) where
  show (PubKey n e) = "Public key: N=" ++ show n ++ " e=" ++ 
                      show e

instance Show a => (Show (PriKey a)) where
  show (PriKey n d) = "Private key: N=" ++ show n ++ " d=" ++
                      show d

genKey :: (RandomGen g, Random a, Integral a) => g -> a -> 
          ((PubKey a, PriKey a), g)
genKey g bits
  | bits <= 4 = error "Too few bits"
  | otherwise = ((PubKey n e, PriKey n d), g'')
  where
    (p,g') = genPrime g bits
    (q,g'') = genDifferentPrime g' bits p
    n = p*q
    phiN = (p-1)*(q-1)
    -- If many bits, this shouldn't be security problem
    -- Remember, this implementation is just for fun, not
    -- for real use
    ePref = min (n `div` 2) 65537
    e = findNextCoprime phiN ePref
    d = e `invMod` phiN


canEncrypt :: Integral a => PubKey a -> a -> Bool
canEncrypt (PubKey n _) m = m > 0 && m < n && coprime n m

canDecrypt :: Integral a => PriKey a -> a -> Bool
canDecrypt (PriKey n _) c = c > 0 && c < n && coprime n c

canSign :: Integral a => PriKey a -> a -> Bool
canSign (PriKey n _) m = m > 0 && m < n && coprime n m

canVerify :: Integral a => PubKey a -> a -> Bool
canVerify (PubKey n _) m = m > 0 && m < n && coprime n m

encrypt :: Integral a => PubKey a -> a -> a
encrypt pk@(PubKey n e) m
  | canEncrypt pk m = powerModulo m e n
  | otherwise       = error "Can't encrypt the message"

decrypt :: Integral a => PriKey a -> a -> a
decrypt pk@(PriKey n d) c
  | canDecrypt pk c = powerModulo c d n
  | otherwise       = error "Can't decrypt the message"

sign :: Integral a => PriKey a -> a -> a
sign pk@(PriKey n e) m
  | canSign pk m = powerModulo m e n
  | otherwise    = error "Can't sign the message"

verify :: Integral a => PubKey a -> a -> a
verify pk@(PubKey n e) m
  | canVerify pk m = powerModulo m e n
  | otherwise      = error "Can't verify the message"


prop_pubThenPri :: Int -> BigInt100000 -> BigInt100000 -> Property
prop_pubThenPri s m b = canEncrypt pubKey m ==> m == m'
  where
    g                   = mkStdGen s
                          -- Not too many bits for performance
    ((pubKey,priKey),_) = genKey g (5 + b `mod` 10)
    c                   = encrypt pubKey m
    m'                  = decrypt priKey c

prop_priThenPub :: Int -> BigInt100000 -> BigInt100000 -> Property
prop_priThenPub s m b = canSign priKey m ==> m == designed
  where
    g                   = mkStdGen s
    ((pubKey,priKey),_) = genKey g (5 + b `mod` 10)
    signed              = sign priKey m
    designed            = verify pubKey signed