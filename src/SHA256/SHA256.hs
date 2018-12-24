
module SHA256.SHA256
(
)
where

import Data.Word
import Data.Bits
import Numeric

import SHA256.Pad
import SHA256.MerkleDamgard
import SHA256.Constants
import SHA256.Compression
import SHA256.MessageSchedule
import SHA256.Types
import Tools


toHex :: (Show a, Integral a) => a -> String
toHex n = showHex n ""

mdIV :: Digest
mdIV = map iv [0..7]

mdComp :: Digest -> Chunk -> Digest
mdComp digest ms = zipWith (+) digest c
  where
    c = comp digest k (wc ms)

inner :: [Chunk] -> Digest
inner = merkleDamgard mdComp mdIV

w8ToW32 :: BitString -> Word32
w8ToW32 = w8ToW32' 24

w8ToW32' :: Int -> BitString -> Word32
w8ToW32' _ []     = 0
w8ToW32' n (b:bs) = (fromIntegral b) `shiftL` n +
                    w8ToW32' (n-8) bs

outer :: BitString -> Digest
outer ms = hashed
  where
    padded = map w8ToW32 (group 4 (pad ms))
    chunks = group 16 padded
    hashed = inner chunks






