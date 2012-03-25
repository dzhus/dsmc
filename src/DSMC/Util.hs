-- | Utility functions.

module DSMC.Util
    ( solveq
    )

where

-- | Solve quadratic equation.
--
-- If less than two roots exist, Nothing is returned.
solveq :: (Double, Double, Double)
       -- ^ Coefficients
       -> Maybe (Double, Double)
solveq (a, b, c)
    | (d >  0) = Just (r - s, r + s)
    | (d <= 0) = Nothing
    where
      d =   b * b - 4 * a * c
      q =   sqrt d
      t =   2 * a
      r = - b / t
      s =   q / t
{-# INLINE solveq #-}
