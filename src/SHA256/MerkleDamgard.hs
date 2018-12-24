
module SHA256.MerkleDamgard
( merkleDamgard
)
where

-- Merkle-Damgård. Given a compression function
-- taking a tag and a message, this gives
-- a function taking arbitrarily long list
-- of messages, using the compression function
-- and making it into a tag
-- Need to supply initial tag/iv.
merkleDamgard :: (t -> m -> t) -> t -> ([m] -> t)
merkleDamgard _ t []     = t
merkleDamgard h t (m:ms) = merkleDamgard h (h t m) ms
