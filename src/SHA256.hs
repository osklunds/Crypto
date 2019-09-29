
module SHA256
( sha256
)
where

import Data.Word
import Data.Bits
import Tools

import SHA256.Padding
import SHA256.Constants
import SHA256.Compression
import SHA256.MessageSchedule
import SHA256.RepConv

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