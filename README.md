
# Crypto

Textbook implementations of cryptographic algorithms in Haskell, just for fun!

## How to run

Change directory to `src/` and type `ghci`. Now load the module you want to run. Some modules require the package `MonadRandom`, which you can install with `cabal install MonadRandom`.

## Modules

- `Math` Discrete mathematics. (Note: there is no top-level `Math` module.
    - `Math.Common` Small helper functions shared by several modules.
    - `Math.GCD` Extended Euclidean Algorithm and inverse modulo.
    - `Math.PowerModulo` Efficient exponentiation done modulo.
    - `Math.Prime` Primality test using Miller Rabin.
    - `Math.BigInt` Data types with `Arbitrary` instances giving larger numbers.
    - `Math.Generation` Generation of primes and coprimes.
- `RSA` RSA key generation and encryption/decryption/signing/verification.
  - `RSA.Example` Example usage of the RSA library.
- `Shamir` Shamir Threshold Secret Sharing Scheme.
  - `Shamir.Example` Example usage of the secret sharing scheme.
- `SHA256` The SHA256 hash function.
  - `Compression` Compression function used by SHA256.
  - `Constants` Round constants and initial hash values.
  - `MerkleDamgard` Function for calculating a Merkle-Damgård style hash function.
  - `MessageSchedule` List for the message schedule array.
  - `Pad` Padding for the pre-processing.
  - `RepConv` Converting between different representations of words and lists of words.
  - `Test` Comparsion between this implementation and a library function.
  - `Types` Synonyms for types used by the different functions.
