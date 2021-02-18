module Types where
newtype LogProb = LogProb (Double -> Double)
newtype Stepsize = Stepsize Double
