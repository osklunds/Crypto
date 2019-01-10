
-- Shared small mathematical functions.

module Math.Common
( divides
)
where

divides :: Integral a => a -> a -> Bool
a `divides` b = b `mod` a == 0