
module SHA256
( sha256
)
where

import Data.Word
import Data.Bits

import SHA256.Pad
import SHA256.MerkleDamgard
import SHA256.Constants
import SHA256.Compression
import SHA256.MessageSchedule
import SHA256.Types
import SHA256.RepConv

import Tools

-- IV to use for Merkle-Damgård
mdIV :: Digest
mdIV = map iv [0..7]

-- Compression function to use for Merkle-Damgård
mdComp :: Digest -> Chunk -> Digest
mdComp digest ms = comp digest k (wc ms)

-- Inner hashing using Merkle-Damgård
inner :: [Chunk] -> Digest
inner = merkleDamgard mdComp mdIV


-- Outer hashing using padding, and then Merkle-Damgård
outer :: BitString -> Digest
outer ms = hashed
  where
    padded = map w8sToW32 $ group 4 $ pad ms
    -- Split padded messages into 512-bit chunks of 32-bit words
    chunks = group 16 padded
    hashed = inner chunks

-- The hash of an ascii string, as a string of hex numbers
sha256 :: String -> String
sha256 = w32sToHex . outer . asciiToW8s
