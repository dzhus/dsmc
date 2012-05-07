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
    | (d >  0) = Just (min r1 r2, max r1 r2)
    | (d <= 0) = Nothing
    where
      d  =   b * b - 4 * a * c
      q  =   sqrt d
      t  =   2 * a
      r  = - b / t
      s  =   q / t
      r1 =   r - s
      r2 =   r + s
{-# INLINE solveq #-}
