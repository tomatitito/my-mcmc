module Kernels.MetropolisHastings where

import Control.Monad.Primitive
import Control.Monad.ST
import Helpers (randomDoubleBetween, randomDoubleBetween')
import System.Random.MWC

-- adapted from https://www.tweag.io/blog/2019-10-25-mcmc-intro1/

-- Transition Kernel is T(x_i+1 | x_i) = proposal * acceptance_probability

proposeST :: Double -> Double -> ST s Double
proposeST x stepsize = randomDoubleBetween' (x - stepsize) (x + stepsize)

acceptOrRejectST :: Double -> (Double -> Double) -> Double -> ST s Double
acceptOrRejectST current logProb proposal = do
  accept <- randomDoubleBetween' 0 1
  if accept < acceptanceProbability current proposal logProb
    then return proposal
    else return current

transitionST :: Double -> (Double -> Double) -> Double -> ST s Double
transitionST current logProb stepsize =
  propose' >>= acceptOrReject'
  where
    propose' = proposeST current stepsize
    acceptOrReject' = acceptOrRejectST current logProb

sampleST :: Double -> (Double -> Double) -> Double -> ST s [Double]
sampleST current logProb stepsize =
<<<<<<< HEAD
  return $
    unsafeInlinePrim $ do
      x <- transitionST current logProb stepsize
      xs <- sampleST x logProb stepsize
      return $! x : xs
=======
  do
    x <- transitionST current logProb stepsize
    xs <- sampleST x logProb stepsize
    return $! x:xs
>>>>>>> 66e33469ff2c79714564e848a8da1a4991995f52

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
