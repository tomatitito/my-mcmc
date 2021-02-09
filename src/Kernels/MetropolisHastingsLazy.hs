module Kernels.MetropolisHastingsLazy where

import Control.Monad.State.Lazy
import System.Random
import qualified Helpers

acceptanceProbability :: (Num a, Num b, Ord b, Floating b) => a -> a -> (a -> b) -> b
acceptanceProbability current proposal logProb =
  min 1 (exp (logProb proposal - logProb current))

propose :: (Num a, UniformRange a) => a -> a -> State StdGen a
propose x stepsize = state $ do
  (proposal, newGen) <- uniformR ((x - stepsize), (x + stepsize))
  return (proposal, newGen)
 
acceptOrReject :: (Num a, Num b, UniformRange b, Floating b, Ord b) => a -> a -> (a -> b) -> State StdGen a 
acceptOrReject current proposal logProb = state $ do
  (rand, newGen) <- uniformR (0.0, 1.0)
  if rand < acceptanceProbability current proposal logProb 
   then return (proposal, newGen)
   else return (current, newGen)

transition :: (Num a, Num b, Ord b, UniformRange a, UniformRange b, Floating b) => a -> (a -> b) -> a -> State StdGen a
transition current logProb stepsize = 
  propose' >>= acceptOrReject'
  where
    propose' = propose current stepsize
    acceptOrReject' = \proposal -> acceptOrReject current proposal logProb

data MhData = MhData {
  n :: Int,
  chain :: [Double],
  gen :: StdGen,
  logProb :: Double -> Double,
  stepsize :: Double
}

simpleSample :: MhData -> MhData
simpleSample dat
  | length (chain dat) == n dat = dat
  | otherwise = simpleSample newDat
  where
    stateVal = transition (head $ chain dat) (logProb dat) (stepsize dat)
    (newVal, newGen) = runState stateVal (gen dat)
    newDat = dat {gen = newGen, chain = newVal : (chain dat)}

d = MhData {n = 5, chain = [1.0], gen = (mkStdGen 42), logProb = Helpers.logProb, stepsize = 0.1}
res = simpleSample d

transitionStateT :: StateT Double (State StdGen) Double
transitionStateT = StateT $ \i -> do
  gen <- get
  let (v', gen') = runState (transition i Helpers.logProb 0.9) gen
  put gen'
  return (i, v') -- State Int (Int,Int)

sampleStateT :: Double -> StdGen -> Int -> ([(Double, Double)], StdGen)
sampleStateT initial gen n =
  let innerState = runStateT transitionStateT initial
  in runState (replicateM n innerState) gen

samples = sampleStateT 0.01 (mkStdGen 42) 6
