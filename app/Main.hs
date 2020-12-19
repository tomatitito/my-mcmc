module Main where

import qualified Data.Vector.Unboxed as U
import Control.Monad.ST (runST, ST)
import System.Random.Stateful

import Kernels.MetropolisHastings

lazyRandomList :: RandomGen g  => g -> [Int]
lazyRandomList g = 
  x : lazyRandomList gNext
  where (x, gNext) = uniform g

main :: IO ()
main = do
  let pureGen = mkStdGen 42
  let pureRandomList = lazyRandomList pureGen
  print $ take 4 $ pureRandomList