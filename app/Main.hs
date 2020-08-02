module Main where

import Helpers
import Kernels.MetropolisHastings

main :: IO ()
main = do
  res <- proposal 2.0 0.3
  print res
