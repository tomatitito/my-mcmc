module Kernels.MetropolisHastingsLazy where

import Control.Monad.State.Lazy
import System.Random
import Types

acceptanceProbability :: LogProb -> Double -> Double -> Double
acceptanceProbability (LogProb logProb) current proposal =
  min 1 (exp (logProb proposal - logProb current))

propose :: Stepsize -> Double -> State StdGen Double
propose (Stepsize stepsize) current = state $ do
  (proposal, newGen) <- uniformR (current - stepsize, current + stepsize)
  return (proposal, newGen)

propose' :: Stepsize -> State (StdGen, Double) Double
propose' (Stepsize stepsize) = do
  (gen, current) <- get
  let (proposal, newGen) = uniformR (current - stepsize, current + stepsize) gen
  put(newGen, current)
  return proposal

acceptOrReject :: LogProb -> Double -> Double -> State StdGen Double
acceptOrReject logProb current proposal = state $ do
  (rand, newGen) <- uniformR (0.0, 1.0)
  if rand < acceptanceProbability logProb current proposal
   then return (proposal, newGen)
   else return (current, newGen)

acceptOrReject' :: LogProb -> Double -> State (StdGen, Double) Double
acceptOrReject' logProb proposal = do
  (gen, current) <- get
  let (rand, newGen) = uniformR (0.0, 1.0) gen
  let nextValue = if rand < acceptanceProbability logProb current proposal
                   then proposal
                   else current
  put (newGen, nextValue)
  return nextValue

transition :: LogProb -> Stepsize -> Double -> State StdGen Double
transition logProb stepsize current =
  propose' >>= acceptOrReject'
  where
    propose' = propose stepsize current
    acceptOrReject' = acceptOrReject logProb current

-- State s a
-- State (StdGen, Double) Double 
transition' :: LogProb -> Stepsize -> State (StdGen, Double) Double
transition' logProb stepsize = propose' stepsize >>= acceptOrReject' logProb
 
transitionStateT :: LogProb -> Stepsize -> StateT Double (State StdGen) Double
transitionStateT logProb stepSize = StateT $ \i -> do
  gen <- get
  let (v', gen') = runState (transition logProb stepSize i) gen
  put gen'
  return (i, v') -- State Double (Double, Double)

sampleStateT :: LogProb -> Stepsize -> Int -> Double -> StdGen -> ([(Double, Double)], StdGen)
sampleStateT logProb stepsize n initial =
  let innerState = runStateT (transitionStateT logProb stepsize) initial
  in runState (replicateM n innerState)

sampleStateT' :: LogProb -> Stepsize -> Int -> Double -> StdGen -> ([Double], (StdGen, Double))
sampleStateT' logProb stepsize n initial gen =
  runState (replicateM n transition'') (gen, initial)
  where transition'' = transition' logProb stepsize
