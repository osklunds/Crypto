
module SecretSum
(
)
where

import System.Random

import Shamir
import Math.NumClass


data Phase1Share a = Phase1Share a a

instance Show a => Show (Phase1Share a) where
  show (Phase1Share s p) = "<Phase1Share. Share = " ++ show s ++ 
                           ", Party = " ++ show p ++ ">"

createPhase1Shares :: (NumClass a, RandomGen g) => 
  a -> ShamirParams a -> g -> ([Phase1Share a], g)
createPhase1Shares x p g = (phase1Shares, g')
  where
    (shamirShares, g') = share x p g
    phase1Shares = map shamirShareToPhase1Share shamirShares

shamirShareToPhase1Share :: ShareIdPair a -> Phase1Share a
shamirShareToPhase1Share (ShareIdPair s p) = Phase1Share s p

-- First is val, second is party
data Phase2Share a = Phase2Share a a

{-
createPhase2Share :: NumClass a => [ShareIdPair a] -> ShamirParams a
 -> Phase2Share a
createPhase2Share [] _ = Phase2Share 0 0
createPhase2Share ((ShareIdPair s _):pairs) p@(ShamirParams n t q) = Phase2Share ((s + createPhase2Share pairs p) `mod` q) 1


createPhase2Share' :: NumClass a => [a] -> a -> a
createPhase2Share' shares = sum shares `mod` q
-}