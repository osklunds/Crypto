
-- Short example demonstrating the RSA library.

module RSA.Example
(
)
where

import System.Random
import Control.Monad.Random.Lazy

import RSA


-- First we create keys for Alice and Bob

type KeysType = (PubKey Integer, PriKey Integer)

aliceKeys = fst (runRand (genKey 20) (mkStdGen 123)) :: KeysType
bobKeys   = fst (runRand (genKey 20) (mkStdGen 456)) :: KeysType

alicePrivate = snd $ aliceKeys
alicePublic  = fst $ aliceKeys
bobPrivate   = snd $ bobKeys
bobPublic    = fst $ bobKeys


-- Then Alice sends a message to Bob

-- Message that Alice has
aliceMessage = 123

-- 335221064521
aliceMessageAtBob = encrypt bobPublic aliceMessage

-- Bob decrypts. 123 is recovered!
bobDecrypted = decrypt bobPrivate aliceMessageAtBob

