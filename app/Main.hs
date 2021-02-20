module Main where

import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as U
import Kernels.MetropolisHastings
import Kernels.MetropolisHastingsLazy
import qualified Helpers as H
import System.Random.MWC hiding (uniform)
import System.Random.Stateful
import Types

main :: IO ()
main = do
  let n = 5
      initial = 1.0
      gen = mkStdGen 42
      stepsize = Stepsize 0.9
      
      stateRes = sampleStateT H.logProb stepsize initial gen n
      stateRes' = sampleStateT' H.logProb stepsize initial gen n
  print stateRes
  print stateRes'
