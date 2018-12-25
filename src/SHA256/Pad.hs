
module SHA256.Pad
( pad
)
where

import Prelude hiding (length, take)
import Data.Word
import Data.Bits
import Numeric

import Tools
import SHA256.Types


-- Returns the index, starting from 0, of the most
-- significant bit. Only works for non-zero bytes.
msb ::Integral a => Word8 -> a
msb b = last [fromIntegral i | i <- [0..7], testBit b i]

-- Appends a 1 bit
addOne :: BitString -> BitString
addOne = (++ [128])


-- Returns the length in bits, of the length the bitstring
-- should have after 0:s have been appended
targetBitLength :: Integral a => a -> a
targetBitLength len
  -- We can fit another 64 bits before exceeding block length 512
  | r <= 448 = q * 512 + 448
  -- We must also add another block
  | r >  448 = q * 512 + 448 + 512
  where
    r = len `mod` 512
    q = len `div` 512

-- Appends 0 bits until the length+64 is a multiple of 512
addZeros :: Word64 -> BitString -> BitString
addZeros len bs = take tarLen $ bs ++ repeat 0
  where
    -- Length in blocks
    tarLen = targetBitLength len `div` 8


-- Returns the length in bits of the bitstring.
-- There must be no zero-bytes leading.
bitLength :: BitString -> Word64
bitLength []     = 0
bitLength (b:bs) = msb b + 1 + 8 * length bs

-- Represents a 64-bit word as a list of 8-bit words
w64ToW8List :: Word64 -> BitString
w64ToW8List w64 = take 8 [fromIntegral (w64 `shiftR` i) 
                         | i <- [56,48..]]


pad :: BitString -> BitString
pad bs = addZeros after1Len (addOne bs) ++ w64ToW8List toUse
  where
    origLen = bitLength bs
    after1Len = bitLength (addOne bs)
    byteLen = origLen `div` 8
    toUse   = if origLen == byteLen * 8
              then origLen
              else (byteLen+1) * 8
