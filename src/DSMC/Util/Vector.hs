{-# LANGUAGE BangPatterns #-}

{-| 

Simple 3-vectors and matrices built atop tuples.

-}

module DSMC.Util.Vector
    ( Vec3
    , Matrix
    , Point
    , origin
    -- * Vector operations
    , (<+>)
    , (<->)
    , (><)
    , (.^)
    , (.*)
    , norm
    , normalize
    , invert
    , distance
    , moveBy
    -- * Matrix operations
    , mxv
    , vxv
    , dotM
    , diag
    , addM
    -- * Cartesian system
    , buildCartesian
    )

where

import Prelude hiding (reverse)


-- | Vector in @R^3@.
type Vec3 = (Double, Double, Double)


-- | Matrix given by its rows.
type Matrix = (Vec3, Vec3, Vec3)


-- | Point in @R^3@.
type Point = Vec3


-- | Origin point @(0, 0, 0)@.
origin :: Point
origin  = (0, 0, 0)


-- | Add two vectors.
(<+>) :: Vec3 -> Vec3 -> Vec3
(<+>) !(x1, y1, z1) !(x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
{-# INLINE (<+>) #-}


-- | Subtract two vectors.
(<->) :: Vec3 -> Vec3 -> Vec3
(<->) !(x1, y1, z1) !(x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
{-# INLINE (<->) #-}


-- | Vec3 cross product.
(><) :: Vec3 -> Vec3 -> Vec3
(><) !(x1, y1, z1) !(x2, y2, z2) =
    (y1 * z2 - y2 * z1, x2 * z1 - x1 * z2, x1 * y2 - x2 * y1)
{-# INLINE (><) #-}


-- | Scale vector.
(.^) :: Vec3 -> Double -> Vec3
(.^) !(x, y, z) !s = (x * s, y * s, z * s)
{-# INLINE (.^) #-}


-- | Vec3 dot product.
(.*) :: Vec3 -> Vec3 -> Double
(.*) !(x1, y1, z1) !(x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2
{-# INLINE (.*) #-}


-- | Generic vector dot product.
--
-- Multiply transpose of first vector by given matrix, then multiply
-- the result by second vector.
dotM :: Vec3 -> Vec3 -> Matrix -> Double
dotM !v1 !v2 !m = v1 .* (m `mxv` v2)
{-# INLINE dotM #-}


-- | Multiply matrix (given by row vectors) and vector
mxv :: Matrix -> Vec3 -> Vec3
mxv !(r1, r2, r3) !v = (r1 .* v, r2 .* v, r3 .* v)
{-# INLINE mxv #-}


-- | Produce matrix with diagonal elements equal to given value.
diag :: Double -> Matrix
diag !d = ((d, 0, 0), (0, d, 0), (0, 0, d))
{-# INLINE diag #-}


-- | Transpose vector and multiply it by another vector, producing a
-- matrix.
vxv :: Vec3 -> Vec3 -> Matrix
vxv !(v11, v12, v13) !v2 = (v2 .^ v11, v2 .^ v12, v2 .^ v13)
{-# INLINE vxv #-}


-- | Euclidean distance between two points.
distance :: Point -> Point -> Double
distance !v1 !v2 = norm (v1 <-> v2)
{-# INLINE distance #-}


-- | Euclidean norm of vector.
norm :: Vec3 -> Double
norm !(x, y, z) = sqrt (x * x + y * y + z * z)
{-# INLINE norm #-}


-- | Produce unit vector with same direction as the original one.
normalize :: Vec3 -> Vec3
normalize !v = v .^ (1 / norm v)
{-# INLINE normalize #-}


-- | Scale vector by -1.
invert :: Vec3 -> Vec3
invert !v = v .^ (-1)
{-# INLINE invert #-}


-- | Move point by velocity vector for given time and return new
-- position.
moveBy :: Point
       -- ^ Current position.
       -> Vec3
       -- ^ Velocity.
       -> Double 
       -- ^ Time step.
       -> Point
moveBy !p !v !t = p <+> (v .^ t)
{-# INLINE moveBy #-}


-- | Add two matrices.
--
-- We could add Applicative instance for Matrix and lift (+) to it.
addM :: Matrix -> Matrix -> Matrix
addM !(r11, r12, r13) !(r21, r22, r23) =
    (r11 <+> r21, r12 <+> r22, r13 <+> r23)
{-# INLINE addM #-}


-- | Build cartesian axes from yaw and pitch with 0 roll. Angles are
-- in radians.
buildCartesian :: Double -> Double -> (Vec3, Vec3, Vec3)
buildCartesian yaw pitch = (u, v, w)
    where u = (cos yaw * cos pitch, sin yaw * cos pitch, sin pitch)
          v = (- (sin yaw), cos yaw, 0)
          w = u >< v
{-# INLINE buildCartesian #-}
