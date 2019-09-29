
module SHA256.Padding
( pad
)
where

import Prelude hiding (length, take)
import Data.Word
import Data.Bits
import Numeric
import Tools

import Math.Divisibility
import SHA256.RepConv

pad :: [Word8] -> [Word8]
pad bs = appendLength bs . appendZeros . appendOne $ bs

appendOne :: [Word8] -> [Word8]
appendOne = (++ [128])

appendZeros :: [Word8] -> [Word8]
appendZeros bs
  | is448Mod512 = bs
  | otherwise   = appendZeros (bs ++ [0])
  where
    is448Mod512 = (length bs * 8) `mod` 512 == 448

appendLength :: [Word8] -> [Word8] -> [Word8]
appendLength bs = (++ (w64ToW8x8 $ length bs * 8))