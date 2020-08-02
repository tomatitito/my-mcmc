module Helpers where

import System.Random.MWC
import System.Random.MWC.Distributions

randomFloat :: IO Float
randomFloat = uniformR (0, 1 :: Float) =<< createSystemRandom

randomDoubleBetween :: Double -> Double -> IO Double
randomDoubleBetween a b = uniformR (a, b) =<< createSystemRandom

randomNormal :: IO Double
randomNormal = standard =<< createSystemRandom
