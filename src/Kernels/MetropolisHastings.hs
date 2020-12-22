module Kernels.MetropolisHastings where

import Control.Monad.Primitive
import Control.Monad.ST
import Helpers (randomDoubleBetween, randomDoubleBetween')
import System.Random.MWC

-- adapted from https://www.tweag.io/blog/2019-10-25-mcmc-intro1/

acceptanceProbability :: Double -> Double -> (Double -> Double) -> Double
acceptanceProbability current proposal logProb =
  min 1 (exp (logProb proposal - logProb current))

propose :: PrimMonad m => Gen (PrimState m) -> Double -> Double -> m Double
propose gen x stepsize =
  uniformR ((x - stepsize), (x + stepsize)) gen

acceptOrReject :: PrimMonad m => Gen (PrimState m ) -> Double -> (Double -> Double) -> Double -> m Double
acceptOrReject gen current logProb proposal =
  do
    accept <- uniformR (0.0, 1.0) gen
    if accept < acceptanceProbability current proposal logProb
      then return accept
      else return proposal

-- Transition Kernel is T(x_i+1 | x_i) = proposal * acceptance_probability
transition :: PrimMonad m => Gen (PrimState m) -> Double -> (Double -> Double) -> Double -> m Double
transition gen current logProb stepsize =
  propose' >>= acceptOrReject'
  where propose' = propose gen current stepsize
        acceptOrReject' = acceptOrReject gen current logProb

sample :: PrimMonad m => Gen (PrimState m) -> Double -> (Double -> Double) -> Double -> m [Double]
sample gen current logProb stepsize = do
  x <- return 1.0 -- transition gen current logProb stepsize
  xs <- sample gen x logProb stepsize
  return $ x : xs
