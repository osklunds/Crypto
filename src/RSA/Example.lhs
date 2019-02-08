
This is a short example demonstrating the RSA library.

> module RSA.Example
> (
> )
> where

> import System.Random
> import Control.Monad.Random.Lazy
> import RSA

First we create keys for Alice and Bob.

> type KeysType = (PubKey Integer, PriKey Integer)
> aliceKeys = fst (runRand (getKey 20) (mkStdGen 123)) :: KeysType
> bobKeys   = fst (runRand (getKey 20) (mkStdGen 456)) :: KeysType

> alicePrivate = snd $ aliceKeys
> alicePublic  = fst $ aliceKeys
> bobPrivate   = snd $ bobKeys
> bobPublic    = fst $ bobKeys

Alice has a message she wants to send.

> aliceMessage = 123

So she encrypts it

> aliceMessageAtBob = encrypt bobPublic aliceMessage

The result, if checking it in GHCi will be 559666469990.

To get the result Bob decrypts the ciphertext.

> bobDecrypted = decrypt bobPrivate aliceMessageAtBob

At we see in GHCi that Bob indeed decrypts the message to 123.

