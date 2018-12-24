
module SHA256.Pad
(
)
where

import Prelude hiding (length, take)
import Data.Word
import Data.Bits
import Numeric

import Tools


data Padded = Padded [Word8]
            deriving (Show)

-- Returns the index, starting from 0, of the most
-- significant bit. Only works for non-zero bytes.
msb ::Integral a => Word8 -> a
msb b = last [fromIntegral i | i <- [0..7], testBit b i]

-- Prepends a 1 bit
addOne :: [Word8] -> [Word8]
addOne []     = [1]
addOne bss@(b:bs) 
  | msb' == 7  = 1:bss
  | otherwise = (setBit b (msb'+1)):bs
  where
    msb' = msb b

-- Returns the length in bits of the bitstring.
-- There must be no zero-bytes leading.
bitLength :: [Word8] -> Word64
bitLength []     = 0
bitLength (b:bs) = msb b + 1 + 8 * length bs

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
addZeros :: [Word8] -> [Word8]
addZeros bs = reverse . take tarLen $ reverse bs ++ repeat 0
  where
    curLen = bitLength bs
    -- Length in blocks
    tarLen = targetBitLength curLen `div` 8

-- Represents a 64-bit word as a list of 8-bit words
w64ToW8List :: Word64 -> [Word8]
w64ToW8List w64 = take 8 [fromIntegral (w64 `shiftR` i) 
                         | i <- [56,48..]]

pad :: [Word8] -> Padded
pad bs = Padded (w64ToW8List origLen ++ (addZeros . addOne) bs)
  where
    origLen = bitLength bs