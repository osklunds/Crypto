
module Tools
( length
, take
, group
)
where

import Prelude hiding (length, take, drop)

length :: Integral a => [b] -> a
length []     = 0
length (a:as) = 1 + length as

take :: Integral a => a -> [b] -> [b]
take _ []     = []
take 0 _      = []
take n (a:as) = a:(take (n-1) as)

drop :: Integral a => a -> [b] -> [b]
drop _ []     = []
drop 0 as     = as
drop n (a:as) = drop (n-1) as

group :: Integral a => a -> [b] -> [[b]]
group _ [] = []
group n l  = (take n l) : (group n (drop n l))