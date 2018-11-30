
{-# LANGUAGE FlexibleInstances #-}

module Math.BigInt
( BigInt1000
, BigInt100000
, BigInt10000000
)
where

import Test.QuickCheck
import System.Random

-- Used to indicate size of arbitrary values
data L1000
data L100000
data L10000000

data BigInt a = BigInt Integer
               deriving Show

instance Arbitrary (BigInt L1000) where
  arbitrary = do
    i <- choose (-1000,1000)
    return $ BigInt i

instance Arbitrary (BigInt L100000) where
  arbitrary = do
    i <- choose (-100000,100000)
    return $ BigInt i

instance Arbitrary (BigInt L10000000) where
  arbitrary = do
    i <- choose (-10000000,10000000)
    return $ BigInt i

-- Short forms
type BigInt1000 = BigInt L1000
type BigInt100000 = BigInt L100000
type BigInt10000000 = BigInt L10000000

unwrap :: BigInt a -> Integer
unwrap (BigInt i) = i

unOp :: (Integer -> Integer) -> BigInt a -> BigInt a
unOp op (BigInt a) = BigInt $ op a

binOp :: (Integer -> Integer -> Integer) -> 
         BigInt a -> BigInt a -> BigInt a
binOp op (BigInt a) (BigInt b) = BigInt (a `op` b)

instance Num (BigInt a) where
  (+)         = binOp (+)
  (-)         = binOp (-)
  (*)         = binOp (*)
  abs         = unOp abs
  signum      = unOp signum
  fromInteger = BigInt

instance Enum (BigInt a) where
  toEnum   = BigInt . toEnum
  fromEnum = fromEnum . unwrap

instance Eq (BigInt a) where
  (BigInt a) == (BigInt b) = a == b

instance Ord (BigInt a) where
  (BigInt a) `compare` (BigInt b) = a `compare` b

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



