
-- Types for integers with Arbitrary instances giving
-- larger numbers than Int and Integer gives.

{-# LANGUAGE FlexibleInstances #-}

module Math.BigInt
( BigInt3
, BigInt5
, BigInt7
)
where

import Test.QuickCheck
import System.Random
import Data.Bits

-- Used to indicate size of arbitrary values. The number
-- indicates how many digits the numbers will get.
data L3
data L5
data L7

data BigInt a = BigInt Integer
               deriving Show

instance Arbitrary (BigInt L3) where
  arbitrary = arbitrarySized (10^3)

instance Arbitrary (BigInt L5) where
  arbitrary = arbitrarySized (10^5)

instance Arbitrary (BigInt L7) where
  arbitrary = arbitrarySized (10^7)

arbitrarySized :: Integer -> Gen (BigInt a)
arbitrarySized n = choose (-n,n) >>= return . BigInt


type BigInt3 = BigInt L3
type BigInt5 = BigInt L5
type BigInt7 = BigInt L7

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
  (.|.) = liftBinInt (.|.)
  bit i = BigInt (bit i)

  (.&.)        = undefined
  xor          = undefined
  complement   = undefined
  shift        = undefined
  rotate       = undefined
  bitSize      = undefined
  bitSizeMaybe = undefined
  isSigned     = undefined
  testBit      = undefined
  popCount     = undefined



