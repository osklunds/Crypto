
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
msb ::Integral a => Word32 -> a
msb b = last [fromIntegral i | i <- [0..31], testBit b i]

-- Prepends a 1 bit
addOne :: BitString -> BitString
addOne []     = [1]
addOne bss@(b:bs) 
  | msb' == 31 = 1:bss
  | otherwise  = (setBit b (msb'+1)):bs
  where
    msb' = msb b

-- Returns the length in bits of the bitstring.
-- There must be no zero-bytes leading.
bitLength :: BitString -> Word64
bitLength []     = 0
bitLength (b:bs) = msb b + 1 + 32 * length bs

-- Returns the length in bits, of the length the bitstring
-- should have after 0:s have been prepended
targetBitLength :: Integral a => a -> a
targetBitLength len
  -- We can fit another 64 bits before exceeding block length 512
  | r <= 448 = q * 512 + 448
  -- We must also add another block
  | r >  448 = q * 512 + 448 + 512
  where
    r = len `mod` 512
    q = len `div` 512

-- Prepends 0 bits until the length+64 is a multiple of 512
addZeros :: BitString -> BitString
addZeros bs = reverse . take tarLen $ reverse bs ++ repeat 0
  where
    curLen = bitLength bs
    -- Length in blocks
    tarLen = targetBitLength curLen `div` 32

-- Represents a 64-bit word as a list of 32-bit words
w64ToW8List :: Word64 -> BitString
w64ToW8List w64 = take 32 [fromIntegral (w64 `shiftR` i) 
                         | i <- [32,0]]

pad :: BitString -> BitString
pad bs = w64ToW8List origLen ++ (addZeros . addOne) bs
  where
    origLen = bitLength bs
