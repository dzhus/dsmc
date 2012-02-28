-- | Utility definitions used in various parts of the program.

module Util

where

type Time = Double

-- | Infinity definition for 'RealFloat'.
infinityP = 1 / 0
infinityN = -infinityP

-- Solve quadratic equation
solveq :: (Double, Double, Double) -> Maybe (Double, Double)
solveq (a, b, c)
    | (d >  0) = Just ((- b - sqrt d) / (2 * a), (- b + sqrt d) / (2 * a))
    | (d <= 0) = Nothing
    where
      d = b * b - 4 * a * c
