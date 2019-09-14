
{-# LANGUAGE ConstraintKinds #-}

module Math.NumClass
( NumClass
)
where

import System.Random
import Data.Bits

-- The generic type used in most places need these three constraints.
type NumClass a = (Bits a, Integral a, Random a)