
module SHA256
( sha256
)
where

import Data.Word
import Data.Bits

import SHA256.Padding
import SHA256.Constants
import SHA256.Compression
import SHA256.MessageSchedule
import SHA256.Types
import SHA256.RepConv

import Tools

processChunks :: Digest -> [Chunk] -> Digest
processChunks d []     = d
processChunks d (c:cs) = processChunks (compress ks ws d) cs
  where
    ks = roundConstants
    ws = messageSchedule c

padAndChunk :: BitString -> [Chunk]
padAndChunk = group 16 . map w8sToW32 . group 4 . pad

-- The hash of an ascii string, as a string of hex numbers
sha256 :: String -> String
sha256 = w32sToHex . processChunks initialHash . padAndChunk . asciiToW8s
