
module SHA256.MessageSchedule
( messageSchedule
)
where

import Data.Word
import Data.Bits

-- chunk is of length 16. So 512 bits in total.
messageSchedule :: [Word32] -> [Word32]
messageSchedule chunk = let ws = chunk ++ map w [16..63]
                            w i = w16 + s0 w15 + w7 + s1 w2
                              where
                                w16 = ws !! (i-16)
                                w15 = ws !! (i-15)
                                w7  = ws !! (i-7)
                                w2  = ws !! (i-2)
                        in ws

s0 :: Word32 -> Word32
s0 w15 = (w15 `rotateR` 7 ) `xor`
         (w15 `rotateR` 18) `xor`
         (w15 `shiftR`  3 )

s1 :: Word32 -> Word32
s1 w2  = (w2  `rotateR` 17) `xor`
         (w2  `rotateR` 19) `xor`
         (w2  `shiftR`  10)