
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


createPhase2Share :: NumClass a => ShamirParams a -> [Phase1Share a]
  -> Phase2Share a
createPhase2Share (ShamirParams n _ q) shares
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
prop_calculateSum g p@(ShamirParams n t q) = sumDirect == sumSecret
  where
    (inputs, g') = randomInputs n q g
    sumDirect = sum inputs `mod` q

    phase2Shares = phase2SharesFromInputsToUse inputs p g'
    sumSecret = calculateSum phase2Shares p

randomInputs :: (NumClass a, RandomGen g) => a -> a -> g -> ([a], g)
randomInputs n q g = (take n $ randomRs (0,q-1) g', g'')
  where
    (g',g'') = split g

phase2SharesFromInputsToUse :: (NumClass a, RandomGen g) =>
  [a] -> ShamirParams a -> g -> [Phase2Share a]
phase2SharesFromInputsToUse inputs p@(ShamirParams n t _) g = shares
  where
    (phase2Shares, g') = phase2SharesFromInputs inputs p g
    parties = randomParties (0,n-1) (t+1,n) g'
    shares = [phase2Shares !! pi | pi <- parties]

phase2SharesFromInputs :: (NumClass a, RandomGen g) => [a] -> ShamirParams a -> g -> ([Phase2Share a], g)
phase2SharesFromInputs inputs p g = (phase2Shares, g')
  where
    phase2Shares = map (createPhase2Share p) phase1SharesReceived
    phase1SharesReceived = transpose phase1SharesCreated
    (phase1SharesCreated, g') = createPhase1SharesFromInputs inputs p g
    
createPhase1SharesFromInputs :: (NumClass a, RandomGen g) =>
  [a] -> ShamirParams a -> g -> ([[Phase1Share a]], g)
createPhase1SharesFromInputs []     _ g = ([], g)
createPhase1SharesFromInputs (i:is) p g = (shares:rest, g'')
  where
    (shares, g') = createPhase1Shares i p g
    (rest, g'')  = createPhase1SharesFromInputs is p g'

randomParties :: (NumClass a, RandomGen g) => 
  (a, a) -> (a, a) -> g -> [a]
randomParties partyIdRange numRange g = parties
  where
    (parties, _)     = randomRUs numParties partyIdRange g'
    (numParties, g') = randomR numRange g