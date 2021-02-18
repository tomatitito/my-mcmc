module Kernels.MetropolisHastings where

import Control.Monad.Primitive
import Control.Monad.ST
import Helpers (randomDoubleBetween, randomDoubleBetween')
import System.Random.MWC
import Types
-- adapted from https://www.tweag.io/blog/2019-10-25-mcmc-intro1/

-- Transition Kernel is T(x_i+1 | x_i) = proposal * acceptance_probability

acceptanceProbability :: LogProb -> Double -> Double -> Double
acceptanceProbability (LogProb logProb) current proposal  =
  min 1 (exp (logProb proposal - logProb current))

propose :: PrimMonad m => Stepsize -> Gen (PrimState m) -> Double ->  m Double
propose (Stepsize stepsize) gen current =
  uniformR (current - stepsize, current + stepsize) gen

acceptOrReject :: PrimMonad m => LogProb -> Gen (PrimState m ) -> Double -> Double -> m Double
acceptOrReject logProb gen current proposal =
  do
    accept <- uniformR (0.0, 1.0) gen
    if accept < acceptanceProbability logProb current proposal
      then return accept
      else return proposal

transition :: PrimMonad m => LogProb -> Stepsize -> Gen (PrimState m) -> Double -> m Double
transition logProb stepsize gen current =
  propose' >>= acceptOrReject'
  where propose' = propose stepsize gen current
        acceptOrReject' = acceptOrReject logProb gen current

sample :: PrimMonad m => LogProb -> Stepsize -> Gen (PrimState m) -> Double -> m [Double]
sample logProb stepsize gen current = do
  x <- transition logProb stepsize gen current
  xs <- sample logProb stepsize gen x
  return $ x : xs
