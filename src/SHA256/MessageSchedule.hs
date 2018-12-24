
module SHA256.MessageSchedule
( wc
)
where

import Data.Word
import Data.Bits

-- List of the message schedule array
ws :: [Word32] -> [Word32]
ws cnk = list
  where
    list = cnk ++ map w [16..63]

    w i = w16 + s0 + w7 + s1
      where
        w16 = list !! (i-16)
        w15 = list !! (i-15)
        w7  = list !! (i-7)
        w2  = list !! (i-2)

        s0 = (w15 `rotateR` 7 ) `xor`
             (w15 `rotateR` 18) `xor`
             (w15 `shiftR`  3 )
        s1 = (w2  `rotateR` 17) `xor`
             (w2  `rotateR` 19) `xor`
             (w2  `shiftR`  10)

-- Message schedule array as a function
wc :: [Word32] -> Word32 -> Word32
wc cnk i = ws cnk !! (fromIntegral i)