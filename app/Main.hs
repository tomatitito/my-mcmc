module Main where

import qualified Data.Vector.Unboxed as U

import Kernels.MetropolisHastings
import System.Random.MWC
import System.Random.Stateful


logProb :: Double -> Double
logProb x = (-0.5) * x**2


ioIsNotLazy :: Double -> IO [Double]
ioIsNotLazy x = do
  xs <- ioIsNotLazy x
  return $ x : xs

main :: IO ()
main = do
  -- in ST 
  -- in IO
   gen <- createSystemRandom
  -- res <- sample gen 10 logProb 2
   res <- ioIsNotLazy 1.0
   print $ take 5 $ res
