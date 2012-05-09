-- | Utility functions.

module DSMC.Util
    ( solveq
    , SquareRoots
    )

where

type SquareRoots = Maybe (Double, Double)

-- | Solve quadratic equation @ax^2 + bx + c = 0@.
--
-- If less than two roots exist, Nothing is returned.
solveq :: Double 
       -- ^ a
       -> Double
       -- ^ b
       -> Double
       -- ^ c
       -> SquareRoots
solveq a b c
    | (d >  0) = Just $! (min r1 r2, max r1 r2)
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
