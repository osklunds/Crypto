
module SHA256.SHA256
(
)
where

import Data.Word
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
mdComp digest ms = comp digest k (wc ms)

inner :: [Chunk] -> Digest
inner = merkleDamgard mdComp mdIV

outer :: BitString -> Digest
outer ms = hashed
  where
    padded = pad ms
    chunks = group 16 padded
    hashed = inner chunks






