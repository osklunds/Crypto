
module SHA256.SHA256
(
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

-- Makes 4 8-bit words into one 32-bit word
w8ToW32 :: [Word8] -> Word32
w8ToW32 = w8ToW32' 24

w8ToW32' :: Int -> BitString -> Word32
w8ToW32' _ []     = 0
w8ToW32' n (b:bs) = (fromIntegral b) `shiftL` n +
                    w8ToW32' (n-8) bs

-- Outer hashing using padding, and then Merkle-Damgård
outer :: BitString -> Digest
outer ms = hashed
  where
    padded = map w8ToW32 $ group 4 $ pad ms
    -- Split padded messages into 512-bit chunks of 32-bit words
    chunks = group 16 padded
    hashed = inner chunks
