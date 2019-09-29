
{-# LANGUAGE PackageImports #-}

module SHA256
( sha256
)
where

import Prelude hiding (length)
import Data.Word
import Data.Bits
import "cryptohash" Crypto.Hash.SHA256
import Data.ByteString (pack, unpack, empty)
import Test.QuickCheck

import SHA256.Padding
import SHA256.Constants
import SHA256.Compression
import SHA256.MessageSchedule
import SHA256.RepConv
import Tools

sha256 :: String -> String
sha256 = w32x8ToHex . processChunks initialHash . chunk . pad . asciiToW8list

-- Splits the padded bit string into chunks, each 512 bits.
chunk :: [Word8] -> [[Word32]]
chunk = group 16 . map w8x4ToW32 . group 4

processChunks :: [Word32] -> [[Word32]] -> [Word32]
processChunks = foldl processChunk

processChunk :: [Word32] -> [Word32] -> [Word32]
processChunk d c = compress ws d
  where
    ws = messageSchedule c

prop_sha256 :: [String] -> Property
prop_sha256 strs = collect len $ sha256 str == sha256ref str
  where
    str = concat strs
    len = length str

sha256ref :: String -> String
sha256ref = w32x8ToHex . map w8x4ToW32 . group 4 . 
            map fromIntegral . unpack . hash . 
            pack . asciiToW8list