
module Shamir.Example
(
)
where

import System.Random
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy

import Shamir

-- Alice has a secret she wants to share.
aliceSecret = 7

-- She works modulo 101
modulo = 101

-- See creates 4 shares out of it, and want the threshold
-- to be 2. That is, at least 3 will have to collaborate.
shares :: [Integer]
shares = fst $ runRand (share aliceSecret 4 2 modulo) $
         mkStdGen 1337

-- She gives share 1 to Bob, share 2 to Charlie, share 3
-- to Dave and share 4 to Eve
bobShare     = shares !! 0
charlieShare = shares !! 1
daveShare    = shares !! 2
eveShare     = shares !! 3

-- Let's suppose Bob, Charlie and Eve meet and want to
-- collaborate to find the secret.
secret = recover [4,1,2] [eveShare, bobShare, charlieShare] modulo

-- Run this program and ghci, and you'll find that the value
-- of secret is 7!

-- What if too few try?
secretMaybe = recover [1,2] [bobShare, charlieShare] modulo

-- It's 81! Just some garbage.
