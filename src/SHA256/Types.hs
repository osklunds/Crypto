
module SHA256.Types
( BitString
, Comp
, Chunk
, RoundWords
, Digest
)
where

import Data.Word

type BitString = [Word8]  -- Arbitrary length
type Comp      = [Word32] -- 8 32-bit words
type Chunk     = [Word32] -- 16 32-bit words, the 512-bit chunk
type RoundWords= [Word32] -- 64 32-bit words, one per round
type Digest    = [Word32] -- 8 32-bit words
