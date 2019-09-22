
This is a short example deonstrating my implementation of Shamir's Secret Sharing.

> module Shamir.Example
> (
> )
> where

> import System.Random
> import Shamir

 Alice has a secret she wants to share.

> aliceSecret = 7

She wants to share it to 4 of her friends and want the threshold to be 2, meaning up to 2 traitors are tolerated. She wants to work modulo 101.

> params = ShamirParams 4 2 101

She now calculates the shares.

> shares :: [ShareIdPair Integer]
> shares = fst $ share aliceSecret params $ mkStdGen 1337

She gives share 1 to Bob, share 2 to Charlie, share 3 to Dave and share 4 to Eve

> bobShare     = shares !! 0
> charlieShare = shares !! 1
> daveShare    = shares !! 2
> eveShare     = shares !! 3

Let's suppose Bob, Charlie and Eve meet and want to collaborate to find the secret.

> secret = recover [bobShare, charlieShare, eveShare] params

Run this program and ghci, and you'll find that the value of secret is 7!

What if too few try?

> secretMaybe = recover [bobShare, daveShare] params

It's 17! Just some garbage.
