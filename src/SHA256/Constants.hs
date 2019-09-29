
module SHA256.Constants
( roundConstants
, initialHash
)
where

import Prelude hiding ((!!), take)
import Data.Word
import Numeric
import Test.QuickCheck

import Math.Prime
import Tools


-- 64 words for 64 rounds
roundConstants :: [Word32]
roundConstants = take 64 $ map roundConstant primes

roundConstant :: Integral a => a -> Word32
roundConstant = ceiling . 
                (subtract 1) . 
                (*2^32) . 
                (**(1.0/3.0)) . 
                fromIntegral

primes :: [Word32]
primes = filter prime [0..]

prop_roundConstants :: Bool
prop_roundConstants = generateString roundConstants == roundConstantsRef

generateString :: (Show a, Integral a) => [a] -> String
generateString ms = concat [toHex m ++ ", " | m <- ms]

roundConstantsRef :: String
roundConstantsRef = "428a2f98, 71374491, b5c0fbcf, e9b5dba5, 3956c25b, 59f111f1, 923f82a4, ab1c5ed5, d807aa98, 12835b01, 243185be, 550c7dc3, 72be5d74, 80deb1fe, 9bdc06a7, c19bf174, e49b69c1, efbe4786, fc19dc6, 240ca1cc, 2de92c6f, 4a7484aa, 5cb0a9dc, 76f988da, 983e5152, a831c66d, b00327c8, bf597fc7, c6e00bf3, d5a79147, 6ca6351, 14292967, 27b70a85, 2e1b2138, 4d2c6dfc, 53380d13, 650a7354, 766a0abb, 81c2c92e, 92722c85, a2bfe8a1, a81a664b, c24b8b70, c76c51a3, d192e819, d6990624, f40e3585, 106aa070, 19a4c116, 1e376c08, 2748774c, 34b0bcb5, 391c0cb3, 4ed8aa4a, 5b9cca4f, 682e6ff3, 748f82ee, 78a5636f, 84c87814, 8cc70208, 90befffa, a4506ceb, bef9a3f7, c67178f2, "


-- 8 words because the hash is 8 words long. (8*32=256)
initialHash :: [Word32]
initialHash = take 8 $ map initialHashComponent primes

initialHashComponent :: Integral a => a -> Word32
initialHashComponent = ceiling . 
                       (subtract 1) . 
                       (*2^32) . 
                       (**(1.0/2.0)) .
                       fromIntegral

prop_initialHash :: Bool
prop_initialHash = generateString initialHash == initialHashRef

initialHashRef :: String
initialHashRef = "6a09e667, bb67ae85, 3c6ef372, a54ff53a, 510e527f, 9b05688c, 1f83d9ab, 5be0cd19, "