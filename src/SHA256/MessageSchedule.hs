
module SHA256.MessageSchedule
( messageSchedule
)
where

import Data.Word
import Data.Bits

s0 :: Word32 -> Word32
s0 w15 = (w15 `rotateR` 7 ) `xor`
         (w15 `rotateR` 18) `xor`
         (w15 `shiftR`  3 )
s1 :: Word32 -> Word32
s1 w2  = (w2  `rotateR` 17) `xor`
         (w2  `rotateR` 19) `xor`
         (w2  `shiftR`  10)

messageSchedule :: [Word32] -> [Word32]
messageSchedule chunk = let list = chunk ++ map ms [16..63]
                            ms i = w16 + s0 w15 + w7 + s1 w2
                              where
                                w16 = list !! (i-16)
                                w15 = list !! (i-15)
                                w7  = list !! (i-7)
                                w2  = list !! (i-2)
                        in list