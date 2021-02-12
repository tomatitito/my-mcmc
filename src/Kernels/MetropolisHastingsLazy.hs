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

propose' :: State (StdGen, Double) Double
propose' = do
  (gen, current) <- get
  let (proposal, newGen) = uniformR (current - stepsize, current + stepsize) gen
  put(newGen, current)
  return proposal
  where
    stepsize = 0.9

acceptOrReject :: Double -> Double -> State StdGen Double
acceptOrReject current proposal = state $ do
  (rand, newGen) <- uniformR (0.0, 1.0)
  if rand < acceptanceProbability current proposal
   then return (proposal, newGen)
   else return (current, newGen)

acceptOrReject' :: Double -> State (StdGen, Double) Double
acceptOrReject' proposal = do
  (gen, current) <- get
  let (rand, newGen) = uniformR (0.0, 1.0) gen
  let nextValue = if rand < acceptanceProbability current proposal
                   then proposal
                   else current
  put (newGen, nextValue)
  return nextValue

transition :: Double -> State StdGen Double
transition current =
  propose' >>= acceptOrReject'
  where
    propose' = propose current
    acceptOrReject' = acceptOrReject current

-- State s a
-- State (StdGen, Double) Double 
transition' :: State (StdGen, Double) Double
transition' = propose' >>= acceptOrReject'
 
transitionStateT :: StateT Double (State StdGen) Double
transitionStateT = StateT $ \i -> do
  gen <- get
  let (v', gen') = runState (transition i) gen
  put gen'
  return (i, v') -- State Double (Double, Double)

sampleStateT :: Double -> StdGen -> Int -> ([(Double, Double)], StdGen)
sampleStateT initial gen n =
  let innerState = runStateT transitionStateT initial
  in runState (replicateM n innerState) gen

sampleStateT' :: Double -> StdGen -> Int -> ([Double], (StdGen, Double))
sampleStateT' initial gen n =
  runState (replicateM n transition') (gen, initial)
