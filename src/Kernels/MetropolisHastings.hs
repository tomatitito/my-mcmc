module Kernels.MetropolisHastings where

import Helpers (randomDoubleBetween)
-- adapted from https://www.tweag.io/blog/2019-10-25-mcmc-intro1/

-- Transition Kernel is T(x_i+1 | x_i) = proposal * acceptance_probability

proposal :: Double -> Double -> IO Double
proposal x stepsize = randomDoubleBetween (x + stepsize) (x - stepsize)


  

 
