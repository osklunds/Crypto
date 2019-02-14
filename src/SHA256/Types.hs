
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SHA256.Types
( CompVars
)
where

import Data.Word
import Tools.Vector

type L8 = Succ ( Succ ( Succ ( Succ ( Succ ( Succ ( Succ ( Succ Zero)))))))
type L16 = L8 `Add` L8

type CompVars = Vector L8 Word32
type Digest   = Vector L8 Word32
type Chunk    = Vector L16 Word32

{-
type BitString = [Word8]  -- Arbitrary length
type Comp      = [Word32] -- 8 32-bit words
type Chunk     = [Word32] -- 16 32-bit words, the 512-bit chunk
type RoundWords= [Word32] -- 64 32-bit words, one for each round
type Digest    = [Word32] -- 8 32-bit words
-}