
-- | Textbook RSA.

module RSA
( getKey
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

import Control.Monad.Random.Lazy
import Control.Monad.Random.Class
import Test.QuickCheck
import System.Random

import Math.Gen
import Math.BigInt
import Math.Divisibility
import Math.PowerModulo
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

-- | b bit keypair.
getKey :: (Random a, Integral a, MonadRandom m) => 
  a -> m (PubKey a, PriKey a)
getKey bits
  | bits <= 4 = error "Too few bits"
  | otherwise = do
      p <- getPrime     bits
      q <- getDiffPrime bits p

      let n     = p*q
          phiN  = (p-1)*(q-1)
          -- If many bits, this shouldn't be security problem.
          ePref = min (n `div` 2) 65537
          e     = nextCoprime phiN ePref
          d     = e `invMod` phiN

      return (PubKey n e, PriKey n d)

-- can n m determines if m can be encrypted/decrypted/
-- signed using modulus n.
can :: Integral a => a -> a -> Bool
can n m = m > 0 && m < n && coprime n m

canEncrypt :: Integral a => PubKey a -> a -> Bool
canEncrypt (PubKey n _) m = can n m

canDecrypt :: Integral a => PriKey a -> a -> Bool
canDecrypt (PriKey n _) c = can n c

canSign :: Integral a => PriKey a -> a -> Bool
canSign = canDecrypt

canVerify :: Integral a => PubKey a -> a -> Bool
canVerify = canEncrypt


encrypt :: Integral a => PubKey a -> a -> a
encrypt pk@(PubKey n e) m
  | canEncrypt pk m = powerModulo m e n
  | otherwise       = error "Can't encrypt the message"

decrypt :: Integral a => PriKey a -> a -> a
decrypt pk@(PriKey n d) c
  | canDecrypt pk c = powerModulo c d n
  | otherwise       = error "Can't decrypt the message"

sign :: Integral a => PriKey a -> a -> a
sign pk@(PriKey n d) m
  | canSign pk m = powerModulo m d n
  | otherwise    = error "Can't sign the message"

verify :: Integral a => PubKey a -> a -> a -> Bool
verify pk@(PubKey n e) m s
  | canVerify pk m = m == powerModulo s e n
  | otherwise      = error "Can't verify the message"


prop_pubThenPri :: StdGen -> BigInt100000 -> BigInt100000 -> Property
prop_pubThenPri g m b = canEncrypt pubKey m ==> m == m'
  where
    bits            = (5 + b `mod` 10)
    (pubKey,priKey) = fst $ runRand (getKey bits) g
    c               = encrypt pubKey m
    m'              = decrypt priKey c

prop_priThenPub :: StdGen -> BigInt100000 -> BigInt100000 -> Property
prop_priThenPub g m b = canSign priKey m ==> verify pubKey m s
  where
    bits            = (5 + b `mod` 10)
    (pubKey,priKey) = fst $ runRand (getKey bits) g
    s               = sign priKey m
