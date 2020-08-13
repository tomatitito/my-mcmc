module Main where

import qualified Data.Vector.Unboxed as U
import Control.Monad.ST (runST, ST)
import Kernels.MetropolisHastings


logProb :: Double -> Double
logProb x = (-0.5) * x**2

main :: IO ()
main = do
--  -- in ST 
--  let res :: ST s [Double]
--      res = sample 0 logProb 2
--  print $ take 10 $ runST res
  -- in IO
  res <- sample 10 logProb 2
  print $ take 5 $ res
