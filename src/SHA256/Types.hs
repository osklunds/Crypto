
module SHA256.Types
( BitString
, CompVars
, Chunk
, RoundWords
, Digest
)
where

import Data.Word

type BitString  = [Word8]  -- Arbitrary length
type CompVars   = [Word32] -- 8
type Chunk      = [Word32] -- 16
type RoundWords = [Word32] -- 64
type Digest     = [Word32] -- 8
