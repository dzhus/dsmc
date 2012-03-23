-- | Utility definitions used in various parts of the program.

module DSMC.Util
    ( infinityP
    , infinityN
    , solveq
    )

where

-- | Infinity definition for 'RealFloat'.
infinityP :: Double
infinityP = 1 / 0

-- | Negative infinity.
infinityN :: Double
infinityN = -infinityP

-- | Solve quadratic equation
solveq :: (Double, Double, Double)
       -- ^ Coefficients
       -> Maybe (Double, Double)
solveq (a, b, c)
    | (d >  0) = Just ((- b - sqrt d) / (2 * a), (- b + sqrt d) / (2 * a))
    | (d <= 0) = Nothing
    where
      d = b * b - 4 * a * c
