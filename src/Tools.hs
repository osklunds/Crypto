
module Tools
( length
, take
)
where

import Prelude hiding (length, take)

length :: Integral a => [b] -> a
length []     = 0
length (a:as) = 1 + length as

take :: Integral a => a -> [b] -> [b]
take _ []     = []
take 0 _      = []
take n (a:as) = a:(take (n-1) as)