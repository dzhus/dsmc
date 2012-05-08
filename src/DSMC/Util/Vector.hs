{-| 

Naive vector implementation. 

-}

module DSMC.Util.Vector
    ( Vector(..)
    , Matrix(..)
    , Point
    -- * Vector operations
    , (<+>)
    , (<->)
    , (><)
    , (.^)
    , (.*)
    , norm
    , normalize
    , distance
    , reverse
    -- * Matrix operations
    , mxv
    , vxv
    , dotM
    , diag
    , addM
    -- * Cartesian system building
    , buildCartesian
    )

where

import Prelude hiding (reverse)


-- | Vector in @R^3@.
data Vector = Vector !Double !Double !Double
              deriving (Eq, Ord, Show)

-- | Matrix given by its rows.
data Matrix = Matrix !Vector !Vector !Vector
              deriving (Eq, Ord, Show)

-- | Point in @R^3@.
type Point = Vector


-- | Add two vectors.
(<+>) :: Vector -> Vector -> Vector
(<+>) (Vector x1 y1 z1) (Vector x2 y2 z2) = 
    Vector (x1 + x2) (y1 + y2) (z1 + z2)
{-# INLINE (<+>) #-}


-- | Subtract two vectors.
(<->) :: Vector -> Vector -> Vector
(<->) (Vector x1 y1 z1) (Vector x2 y2 z2) = 
    Vector (x1 - x2) (y1 - y2) (z1 - z2)
{-# INLINE (<->) #-}


-- | Vector cross product.
(><) :: Vector -> Vector -> Vector
(><) (Vector x1 y1 z1) (Vector x2 y2 z2) = 
    Vector (y1 * z2 - y2 * z1) (x2 * z1 - x1 * z2) (x1 * y2 - x2 * y1)
{-# INLINE (><) #-}


-- | Scale vector.
(.^) :: Vector -> Double -> Vector
(.^) (Vector x y z) s = Vector (x * s) (y * s) (z * s)
{-# INLINE (.^) #-}


-- | Vector dot product.
(.*) :: Vector -> Vector -> Double
(.*) (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
{-# INLINE (.*) #-}


-- | Generic vector dot product.
--
-- Multiply transpose of first vector by given matrix, then multiply
-- the result by second vector.
dotM :: Vector -> Vector -> Matrix -> Double
dotM v1 v2 m = v1 .* (m `mxv` v2)
{-# INLINE dotM #-}


-- | Multiply matrix (given by row vectors) and vector
mxv :: Matrix -> Vector -> Vector
mxv (Matrix r1 r2 r3) v = Vector (r1 .* v) (r2 .* v) (r3 .* v)
{-# INLINE mxv #-}


-- | Produce matrix with diagonal elements equal to given value.
diag :: Double -> Matrix
diag d = Matrix (Vector d 0 0) (Vector 0 d 0) (Vector 0 0 d)
{-# INLINE diag #-}


-- | Transpose vector and multiply it by another vector, producing a
-- matrix.
vxv :: Vector -> Vector -> Matrix
vxv v1@(Vector v11 v12 v13) v2 = Matrix (v2 .^ v11) (v2 .^ v12) (v2 .^ v13)
{-# INLINE vxv #-}


-- | Euclidean distance between two points.
distance :: Point -> Point -> Double
distance v1 v2 = norm (v1 <-> v2)
{-# INLINE distance #-}


-- | Euclidean norm of vector.
norm :: Vector -> Double
norm (Vector x y z) = sqrt (x * x + y * y + z * z)
{-# INLINE norm #-}


-- | Produce unit vector with same direction as the original one.
normalize :: Vector -> Vector
normalize v = v .^ (1 / norm v)
{-# INLINE normalize #-}


-- | Scale vector by -1.
reverse :: Vector -> Vector
reverse v = v .^ (-1)
{-# INLINE reverse #-}


-- | Add two matrices.
--
-- We could add Applicative instance for Matrix and lift (+) to it.
addM :: Matrix -> Matrix -> Matrix
addM (Matrix r11 r12 r13) (Matrix r21 r22 r23) = 
    Matrix (r11 <+> r21) (r12 <+> r22) (r13 <+> r23)
{-# INLINE addM #-}


-- | Build an ortogonal vector parallel to xy plane (rotated CCW
-- around Oz)
horizontalShifter :: Vector -> Vector
horizontalShifter n@(Vector x y z) 
    | z == 0 = (Vector 0 0 1) >< n
    | x == 0 && y == 0 = (Vector 1 0 0) >< n
    | otherwise = (Vector x y 0) >< n


-- | Build cartesian axes from Ox vector so that Oz belongs to plane
-- perpendicular to Oxy (i.e. roll = 0 degree).
buildCartesian :: Vector -> (Vector, Vector, Vector)
buildCartesian n = (normalize n, normalize v, normalize w)
    where v = horizontalShifter n
          w = v >< n
