module Main where

import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as U
import Kernels.MetropolisHastings
import System.Random.MWC hiding (uniform)
import System.Random.Stateful

lazyRandomList :: RandomGen g => g -> [Int]
lazyRandomList g =
  x : lazyRandomList gNext
  where
    (x, gNext) = uniform g

lazyRandomListWithState :: Int -> State StdGen [Int]
lazyRandomListWithState = (`replicateM` randomInt)
  where
    randomInt :: State StdGen Int
    randomInt = state $ uniform

ioIsNotLazy :: Double -> IO [Double]
ioIsNotLazy x = do
  xs <- ioIsNotLazy x
  return $ x : xs

main :: IO ()
main = do
  let pureGen = mkStdGen 42
  let pureRandomList = lazyRandomList pureGen
  -- not so nice: get the list like this
  let lazyStateList = evalState (lazyRandomListWithState 5) pureGen
  print $ take 4 $ pureRandomList
  print lazyStateList
