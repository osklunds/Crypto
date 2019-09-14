
This is a short example demonstrating the RSA library.

> module RSA.Example
> (
> )
> where

> import System.Random
> import Data.Maybe
> import RSA

First we create keys for Alice and Bob.

> type KeysType = (PubKey Integer, PriKey Integer)
> aliceKeys = fst ((randomKey 20) (mkStdGen 123)) :: KeysType
> bobKeys   = fst ((randomKey 20) (mkStdGen 456)) :: KeysType

> alicePrivate = snd $ aliceKeys
> alicePublic  = fst $ aliceKeys
> bobPrivate   = snd $ bobKeys
> bobPublic    = fst $ bobKeys

Alice has a message she wants to send.

> aliceMessage = 123

So she encrypts it

> aliceMessageAtBob = encrypt bobPublic aliceMessage

The result, if checking it in GHCi will be `Just 559666469990`. The `Just` part means that it was possible to encrypt that particular message.

To get the result Bob decrypts the ciphertext.

> bobDecrypted = decrypt bobPrivate $ fromJust aliceMessageAtBob

And we see in GHCi that Bob indeed decrypts the message to `Just 123`.
