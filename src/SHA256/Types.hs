
module SHA256.Types
( ByteString
, CompVars
, Chunk
, RoundWords
, Digest
)
where

import Data.Word

type ByteString = [Word8]  -- Arbitrary length
type CompVars   = [Word32] -- 8
type Chunk      = [Word32] -- 16
type RoundWords = [Word32] -- 64
type Digest     = [Word32] -- 8
