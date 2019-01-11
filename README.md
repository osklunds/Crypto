
# Crypto

Textbook implementations of cryptographic algorithms in Haskell, just for fun! I have implemented textbook RSA, Shamir's Secret Sharing Scheme and SHA256.

## How to run

Change directory to `src/` and type `ghci`. Now load the module you want to run. Some modules require the package `MonadRandom`, which you can install with `cabal install MonadRandom`.

## Modules overview

![](misc/module_overview.png)

- `Math` Discrete mathematics. (Note: there is no top-level `Math` module.) Used by the other modules.
- `RSA` Textbook RSA cryptography and signatures.
- `Shamir` Shamir's Secret Sharing Scheme.
- `SHA256` The SHA256 hash function.

### Math

The main stars of the math module are

- `GCD` Extended Euclidean Algorithm and related.
- `PowerModulo` Modular exponentiation done efficiently.
- `Prime` Primality testing using the Miller-Rabin test.
- `Gen` Generation of coprimes and primes.

The minor modules are

- `BigInt` Data types with `Arbitrary` instances giving larger numbers.
- `Common` Small auxillary functions used by several modules.

### RSA

`Example` Example usage of the RSA library.

### Shamir

`Example` Example usage of the secret sharing scheme.

### SHA256

- `Compression` Compression function used by SHA256.
- `Constants` Round constants and initial hash values.
- `MerkleDamgard` Function for calculating a Merkle-Damgård style hash function.
- `MessageSchedule` List for the message schedule array.
- `Pad` Padding for the pre-processing.
- `RepConv` Converting between different representations of words and lists of words.
- `Types` Synonyms for types used by the different functions.
- `Test` Comparsion between this implementation and a library function. This module requires the package `Crypto.Hash.SHA256` which can be installed with `cabal install cryptohash`.