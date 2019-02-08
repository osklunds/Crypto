
-- | Types for integers with Arbitrary instances giving
-- larger numbers than Int and Integer gives.

{-# LANGUAGE FlexibleInstances #-}

module Math.BigInt
( BigInt1000
, BigInt100000
, BigInt10000000
)
where

import Test.QuickCheck
import System.Random
import Data.Bits

-- Used to indicate size of arbitrary values.
data L1000
data L100000
data L10000000

data BigInt a = BigInt Integer
               deriving Show

arbitrarySize :: Integer -> Gen (BigInt a)
arbitrarySize n = choose (-n,n) >>= return . BigInt

instance Arbitrary (BigInt L1000) where
  arbitrary = arbitrarySize 1000

instance Arbitrary (BigInt L100000) where
  arbitrary = arbitrarySize 100000

instance Arbitrary (BigInt L10000000) where
  arbitrary = arbitrarySize 10000000

-- | Integer with Arbitrary instance giving between -1000 and 1000.
type BigInt1000 = BigInt L1000
type BigInt100000 = BigInt L100000
type BigInt10000000 = BigInt L10000000

unwrap :: BigInt a -> Integer
unwrap (BigInt i) = i

liftUnInt :: (Integer -> Integer) -> 
             (BigInt a -> BigInt a)
liftUnInt op (BigInt a) = BigInt $ op a

liftBinInt :: (Integer -> Integer -> Integer) -> 
              (BigInt a -> BigInt a -> BigInt a)
liftBinInt op (BigInt a) (BigInt b) = BigInt $ a `op` b

liftBinGen :: (Integer -> Integer -> b) ->
              (BigInt a -> BigInt a -> b)
liftBinGen op (BigInt a) (BigInt b) = a `op` b

instance Num (BigInt a) where
  (+)         = liftBinInt (+)
  (-)         = liftBinInt (-)
  (*)         = liftBinInt (*)
  abs         = liftUnInt abs
  signum      = liftUnInt signum
  fromInteger = BigInt

instance Enum (BigInt a) where
  toEnum   = BigInt . toEnum
  fromEnum = fromEnum . unwrap

instance Eq (BigInt a) where
  (==) = liftBinGen (==)

instance Ord (BigInt a) where
  compare = liftBinGen compare

instance Real (BigInt a) where
  toRational = toRational . unwrap

instance Integral (BigInt a) where
  toInteger                       = unwrap
  (BigInt a) `quotRem` (BigInt b) = let (a',b') = a `quotRem` b
                                    in  (BigInt a', BigInt b')

instance Random (BigInt a) where
  randomR (BigInt a, BigInt b) g = let (r, g') = randomR (a, b) g
                                   in  (BigInt r, g')
  random g                       = let (r, g') = random g
                                   in  (BigInt r, g')

instance Bits (BigInt a) where
  (BigInt a) `shiftL` i = BigInt (a `shiftL` i)
  (BigInt a) .|. (BigInt b) = BigInt (a .|. b)
  bit i = BigInt (bit i)

  (.&.) = undefined
  xor = undefined
  complement = undefined
  shift = undefined
  rotate = undefined
  bitSize = undefined
  bitSizeMaybe = undefined
  isSigned = undefined
  testBit = undefined
  popCount = undefined



