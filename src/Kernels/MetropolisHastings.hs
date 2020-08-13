module Kernels.MetropolisHastings where

import Control.Monad.Primitive
import Control.Monad.ST
import System.Random.MWC
import Helpers (randomDoubleBetween, randomDoubleBetween')

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

  where propose' = proposeST current stepsize
        acceptOrReject' = acceptOrRejectST current logProb

sampleST :: Double -> (Double -> Double) -> Double -> ST s [Double]
sampleST current logProb stepsize =
  return $ unsafeInlinePrim $ do
    x <- transitionST current logProb stepsize
    xs <- sampleST x logProb stepsize
    return $! x:xs

acceptanceProbability :: Double -> Double -> (Double -> Double) -> Double
acceptanceProbability current proposal logProb =
  min 1 (exp (logProb proposal - logProb current))

propose :: PrimMonad m => Double -> Double -> m Double
propose x stepsize = return $ unsafeInlinePrim $ do
  gen <- createSystemRandom
  uniformR ((x - stepsize), (x + stepsize)) gen

acceptOrReject :: PrimMonad m => Double -> (Double -> Double) -> Double -> m Double
acceptOrReject current logProb proposal =
  return $ unsafeInlinePrim $ do
    accept <- uniformR (0.0, 1.0) =<< createSystemRandom
    if accept < acceptanceProbability current proposal logProb
      then return accept
      else return proposal

transition :: PrimMonad m => Double -> (Double -> Double) -> Double -> m Double
transition current logProb stepsize =
  propose' >>= acceptOrReject'

  where propose' = propose current stepsize
        acceptOrReject' = acceptOrReject current logProb

sample :: PrimMonad m => Double -> (Double -> Double) -> Double -> m [Double]
sample current logProb stepsize = do
  x <- transition current logProb stepsize
  -- TODO : Wwhy is runST necessary?
  return $! x: (runST $ sample x logProb stepsize)
