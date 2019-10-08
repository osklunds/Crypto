
This is a short example demonstrating my implementation of SecretSum.

> module SecretSum.Example
> (
> )
> where

> import System.Random
> import SecretSum
> import Shamir

Alice, Bob and Charlie each have their secret number. They want to jointly compute the sum without revealing their individual inputs. First they decide on the parameters to use.

> params = ShamirParams 3 2 101

Since t=2, all three need to participate to retrieve the final sum.

I prefix a variable with a name to make it clear that the variable in question is only known at that party.

> alice_number = 7 :: Integer
> bob_number = 3 :: Integer
> charlie_number = 4 :: Integer

All of them create their phase 1 shares and hand them out.

> alice_gen = mkStdGen 123
> (alice_phase1Shares, _) = createPhase1Shares alice_number params alice_gen
> [alice_alicePhase1Share, bob_alicePhase1Share, charlie_alicePhase1Share] = alice_phase1Shares

> bob_gen = mkStdGen 456
> (bob_phase1Shares, _) = createPhase1Shares bob_number params bob_gen
> [alice_bobPhase1Share, bob_bobPhase1Share, charlie_bobPhase1Share] = bob_phase1Shares

> charlie_gen = mkStdGen 789
> (charlie_phase1Shares, _) = createPhase1Shares charlie_number params charlie_gen
> [alice_charliePhase1Share, bob_charliePhase1Share, charlie_charliePhase1Share] = charlie_phase1Shares

> alice_receivedShares = [alice_alicePhase1Share, alice_bobPhase1Share, alice_charliePhase1Share]
> bob_receivedShares = [bob_alicePhase1Share, bob_bobPhase1Share, bob_charliePhase1Share]
> charlie_receivedShares = [charlie_alicePhase1Share, charlie_bobPhase1Share, charlie_charliePhase1Share]

With the shares from the other parties received, they calculate the phase 2 shares.

> alicePhase2Share = createPhase2Share params alice_receivedShares
> bobPhase2Share = createPhase2Share params bob_receivedShares
> charliePhase2Share = createPhase2Share params charlie_receivedShares

These phase 2 shares are sent to everyone. So as an example, we let Alice calculate the sum.

> allShares = [alicePhase2Share, bobPhase2Share, charliePhase2Share]
> sumByAlice = calculateSum allShares params

If we calculate the sum directly instead (which none of the parties can do).

> sumDirect = alice_number + bob_number + charlie_number

They are the same! But note that Alice, Bob and Charlie managed to calculate this sum without revealing their individual inputs.

Suppose that, for some reason, Charlie doesn't send his phase 2 share. What happens if Alice tries to calculate the sum anyway?

> sumAttemptByAlice = calculateSum [alicePhase2Share, bobPhase2Share] params

This will yield 93! Nonsense. That's because the Shamir params were specified with t = 2, which means at least 3 parties need to collaborate to find the final sum.
