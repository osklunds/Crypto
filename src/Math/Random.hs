
module Math.Random
( randomR_st
, eval
)
where

import Control.Monad.State
import System.Random

randomR_st :: (Integral a, Random a, RandomGen g) =>
  (a, a) -> State g a
randomR_st = state . randomR

eval :: State StdGen a -> a
eval c = fst $ runState c (mkStdGen 11)