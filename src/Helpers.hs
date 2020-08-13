{-# LANGUAGE ScopedTypeVariables #-}
module Helpers where


import Control.Monad.Primitive
import Control.Monad.ST
import System.Random.MWC (uniformR, createSystemRandom, initialize, create, Gen, GenST, Variate, uniform)
import System.Random.MWC.Distributions

randomFloat :: IO Float
randomFloat = uniformR (0, 1 :: Float) =<< createSystemRandom

randomFloat'' :: ST s Float
randomFloat'' = uniformR (0, 1) =<< create -- :: ST s Float

randomFloat''' :: Gen s -> ST s Float
randomFloat''' g = uniformR (0, 1) g

randomDoubleBetween :: Double -> Double -> IO Double
randomDoubleBetween a b = uniformR (a, b) =<< createSystemRandom

randomDoubleBetween' :: Double -> Double -> ST s Double
--see https://stackoverflow.com/questions/26448677/generation-of-infinite-list-of-ints-with-mwc-random
randomDoubleBetween' low high = 
  return $ unsafeInlinePrim $ do
    gen <- createSystemRandom
    -- for debugging use:
    -- gen <- create :: ST s (Gen s)
    uniformR (low, high) gen

randomNormal :: IO Double
randomNormal = standard =<< createSystemRandom
