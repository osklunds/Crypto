
module Math.Random
( getRandomR
, getRandom
, eval
, getRandomList
, getRandomListR
)
where

import Control.Monad.State
import System.Random

getRandom :: (Random a, RandomGen g) => State g a
getRandom = state random

getRandomR :: (Random a, RandomGen g) =>
  (a, a) -> State g a
getRandomR = state . randomR

eval :: State StdGen a -> a
eval c = fst $ runState c (mkStdGen 11)

getRandomList :: (Integral a, Random b, RandomGen g) =>
  a -> State g [b]
getRandomList n = replicateM (fromIntegral n) getRandom

getRandomListR :: (Integral a, Random a, RandomGen g) =>
  a -> (a, a) -> State g [a]
getRandomListR n r = replicateM (fromIntegral n) $ getRandomR r