module Main where

import qualified Data.Vector.Unboxed as U
import Kernels.MetropolisHastings
import System.Random.MWC hiding (uniform)
import System.Random.Stateful 

import Kernels.MetropolisHastings

lazyRandomList :: RandomGen g  => g -> [Int]
lazyRandomList g = 
  x : lazyRandomList gNext
  where (x, gNext) = uniform g


ioIsNotLazy :: Double -> IO [Double]
ioIsNotLazy x = do
  xs <- ioIsNotLazy x
  return $ x : xs

main :: IO ()
main = do
  let pureGen = mkStdGen 42
  let pureRandomList = lazyRandomList pureGen
  print $ take 4 $ pureRandomList
