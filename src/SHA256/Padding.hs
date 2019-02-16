
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
import SHA256.Types
import SHA256.RepConv

appendOne :: ByteString -> ByteString
appendOne = (++ [128])

appendZeros :: ByteString -> ByteString
appendZeros bs
  | isMultiple = bs
  | otherwise  = appendZeros (bs ++ [0])
  where
    isMultiple = 512 `divides` (length bs * 8 + 64)

appendLength :: ByteString -> ByteString -> ByteString
appendLength bs = (++ (w64ToW8List $ 8 * length bs))

pad :: ByteString -> ByteString
pad bs = appendLength bs . appendZeros . appendOne $ bs
