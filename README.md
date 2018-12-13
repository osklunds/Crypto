
# Crypto

Textbook implementations of cryptographic algorithms in Haskell, just for fun!

## How to run

Change directory to `src/` and type `ghci`. Now load the module you want to run.

## Modules

- `Math` Discrete mathematics. (Note: there is no top-level `Math` module.
    - `Math.Common` Small helper functions shared by several modules
    - `Math.GCD` Extended Euclidean Algorithm and inverse modulo
    - `Math.PowerModulo` Efficient exponentiation done modulo
    - `Math.Prime` Primality test using Miller Rabin
    - `Math.BigInt` Data types with `Arbitrary` instances giving larger numbers
    - `Math.Generation` Generation of primes and coprimes
- `RSA` RSA key generation and encryption/decryption/signing/verification
  - `RSA.Example` Example usage of the RSA library
- `Shamir` Shamir Threshold Secret Sharing Scheme.
  - `Shamir.Example` Example usage of the secret sharing scheme.
