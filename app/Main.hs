module Main where

import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as U
import Kernels.MetropolisHastings
import Kernels.MetropolisHastingsLazy
import qualified Helpers as H
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
  let n = 5
      initial = 1.0
      gen = mkStdGen 42
      d = MhData {n = n, chain = [initial], gen = gen, logProb = H.logProb, stepsize = 0.1}
      
      simpleRes = simpleSample d
      stateRes = sampleStateT initial gen n
  print (chain simpleRes)
  print stateRes
