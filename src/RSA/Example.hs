
module RSA.Example
(
)
where

import System.Random

import RSA


-- First we create keys for Alice and Bob

type KeysType = ((PubKey Integer, PriKey Integer), StdGen)

aliceKeys = genKey (mkStdGen 123) 20 :: KeysType
bobKeys   = genKey (mkStdGen 456) 20 :: KeysType

alicePrivate = snd . fst $ aliceKeys
alicePublic  = fst . fst $ aliceKeys
bobPrivate   = snd . fst $ bobKeys
bobPublic    = fst . fst $ bobKeys


-- Then Alice sends a message to Bob

-- Message that Alice has
aliceMessage = 123

-- 335221064521
aliceMessageAtBob = encrypt bobPublic aliceMessage

-- Bob decrypts. 123 is recovered!
bobDecrypted = decrypt bobPrivate aliceMessageAtBob