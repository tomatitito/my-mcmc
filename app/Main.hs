module Main where

import qualified Data.Vector.Unboxed as U
import Kernels.MetropolisHastingsLazy
import System.Random.MWC hiding (uniform)
import System.Random.Stateful 
import Control.Monad.State.Lazy (evalState)


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
  let gen = mkStdGen 42
      val = evalState (propose (1.0 :: Double) 0.5) gen
      logProb x = -0.5 * x ** 2 
      val2 = evalState (acceptOrReject (0.3 :: Double) 0.4 logProb) gen
  print val2
--  let pureGen = mkStdGen 42
--  let pureRandomList = lazyRandomList pureGen
--  print $ take 4 $ pureRandomList
