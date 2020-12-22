module Kernels.MetropolisHastingsLazy where

import Control.Monad.State.Lazy
import System.Random

acceptanceProbability :: (Num a, Num b, Ord b, Floating b) => a -> a -> (a -> b) -> b
acceptanceProbability current proposal logProb =
  min 1 (exp (logProb proposal - logProb current))

-- the generator isn't even an argument
-- instead apply propose...
-- and then apply evalState to the result
-- and pass a generator to evalState
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