module Kernels.MetropolisHastingsLazy where

import Control.Monad.State.Lazy
import System.Random
import qualified Helpers

acceptanceProbability :: Double -> Double -> Double
acceptanceProbability current proposal =
  min 1 (exp (Helpers.logProb proposal - Helpers.logProb current))

propose :: Double -> State StdGen Double
propose x = state $ do
  (proposal, newGen) <- uniformR (x - stepsize, x + stepsize)
  return (proposal, newGen)
  where
    stepsize = 0.9

acceptOrReject :: Double -> Double -> State StdGen Double
acceptOrReject current proposal = state $ do
  (rand, newGen) <- uniformR (0.0, 1.0)
  if rand < acceptanceProbability current proposal
   then return (proposal, newGen)
   else return (current, newGen)

transition :: Double -> State StdGen Double
transition current =
  propose' >>= acceptOrReject'
  where
    propose' = propose current
    acceptOrReject' = acceptOrReject current

transitionStateT :: StateT Double (State StdGen) Double
transitionStateT = StateT $ \i -> do
  gen <- get
  let (v', gen') = runState (transition i) gen
  put gen'
  return (i, v') -- State Double (Double, Double)

-- State s a
-- State (StdGen, Double) Double 
transition' :: Double -> State (StdGen, Double) Double
transition' = undefined


sampleStateT :: Double -> StdGen -> Int -> ([(Double, Double)], StdGen)
sampleStateT initial gen n =
  let innerState = runStateT transitionStateT initial
  in runState (replicateM n innerState) gen
