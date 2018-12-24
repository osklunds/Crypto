
module SHA256.Types
( BitString
, Comp
)
where

import Data.Word

type BitString = [Word32] -- Arbitrary length
type Comp      = [Word32] -- 8 32-bit words
