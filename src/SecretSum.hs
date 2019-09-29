
module SecretSum
( Phase1Share(..)
, createPhase1Shares
, createPhase2Share
, calculateSum
)
where

import Prelude hiding (length, take, (!!))
import Data.List hiding (length, take, (!!))
import System.Random
import Test.QuickCheck

import Shamir
import Math.NumClass
import Math.BigInt
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
  | otherwise          = Phase2Share s p
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

prop_calculateSum :: StdGen -> ShamirParams BigInt3 -> Bool
prop_calculateSum g p@(ShamirParams n t q) = sumSecret == sumDirect
  where
    (g1,g2) = split g
    inputs = take n $ randomRs (0,q-1) g1
    (phase1SharesCreatedPerParty, g3) = createPhase1SharesFromInputs inputs p g2
    phase1SharesReceivedPerParty = transpose phase1SharesCreatedPerParty
    phase2SharesPerParty = map (\shares -> createPhase2Share shares p) phase1SharesReceivedPerParty
    (numParticipants, g4) = randomR (t+1,n) g3
    (finalParties, _) = randomRUs numParticipants (0,n-1) g4
    finalShares = [phase2SharesPerParty !! pi | pi <- finalParties]
    sumSecret = calculateSum finalShares p
    sumDirect = sum inputs `mod` q



createPhase1SharesFromInputs :: (NumClass a, RandomGen g) =>
  [a] -> ShamirParams a -> g -> ([[Phase1Share a]], g)
createPhase1SharesFromInputs []     _ g = ([], g)
createPhase1SharesFromInputs (i:is) p g = (shares:rest, g'')
  where
    (shares, g') = createPhase1Shares i p g
    (rest, g'')  = createPhase1SharesFromInputs is p g'