
module SecretSum
(
)
where

import Prelude hiding (length)
import System.Random

import Shamir
import Math.NumClass
import Tools

-- First is val, second is party
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

instance Show a => Show (Phase2Share a) where
  show (Phase2Share s p) = "<Phase2Share. Share = " ++ show s ++ 
                           ", Party = " ++ show p ++ ">"


createPhase2Share :: NumClass a => [Phase1Share a] -> ShamirParams a
  -> Phase2Share a
createPhase2Share shares (ShamirParams n _ q)
  | length shares /= n = error "Need shares from all parties"
  | otherwise          = Phase2Share p s
  where
    p = partyFromShares shares
    s = sum [s | (Phase1Share s _) <- shares] `mod` q

-- Finds the single party who the shares belong to. If multiple
-- parties, will exit the program.
partyFromShares :: NumClass a => [Phase1Share a] -> a
partyFromShares []                = error "No shares"
partyFromShares [Phase1Share _ p] = p
partyFromShares ((Phase1Share _ p):shares)
  | p == p'   = p
  | otherwise = error "Different parties"
  where
    p' = partyFromShares shares

calculateSum :: NumClass a => [Phase2Share a] -> ShamirParams a -> a
calculateSum shares (ShamirParams _ _ q) = summed
  where
    summed = sum (zipWith (*) bis sis) `mod` q
    bis = map (beta pis q) pis
    pis = map (\(Phase2Share _ pi) -> pi) shares
    sis = map (\(Phase2Share si _) -> si) shares
