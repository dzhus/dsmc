{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

{-| 

Simple vectors and matrices.

All functions and datatypes are strict.

-}

module DSMC.Util.Vector
    ( Vec3(..)
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
    , moveBy
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

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import Prelude hiding (reverse)


-- | Vector in @R^3@.
data Vec3 = Vec3 !Double !Double !Double
              deriving (Eq, Ord, Show)

newtype instance VU.MVector s Vec3 = MV_Vec3 (VU.MVector s Vec3)
newtype instance VU.Vector    Vec3 = V_Vec3  (VU.Vector    Vec3)

instance VGM.MVector VU.MVector Vec3 where
   {-# INLINE basicLength #-}
   basicLength (MV_Vec3 v) = VGM.basicLength v
   {-# INLINE basicUnsafeSlice #-}
   basicUnsafeSlice i n            = VGM.basicUnsafeSlice i n
   {-# INLINE basicOverlaps #-}
   basicOverlaps                   = VGM.basicOverlaps
   {-# INLINE basicUnsafeNew #-}
   basicUnsafeNew                  = VGM.basicUnsafeNew
   {-# INLINE basicUnsafeReplicate #-}
   basicUnsafeReplicate n          = VGM.basicUnsafeReplicate n
   {-# INLINE basicUnsafeRead #-}
   basicUnsafeRead (MV_Vec3 mv)    = VGM.basicUnsafeRead mv
   {-# INLINE basicUnsafeWrite #-}
   basicUnsafeWrite (MV_Vec3 mv) i = VGM.basicUnsafeWrite mv i
   {-# INLINE basicClear #-}
   basicClear                      = VGM.basicClear
   {-# INLINE basicSet #-}
   basicSet (MV_Vec3 mv)           = VGM.basicSet mv
   {-# INLINE basicUnsafeCopy #-}
   basicUnsafeCopy                 = VGM.basicUnsafeCopy
   {-# INLINE basicUnsafeGrow #-}
   basicUnsafeGrow (MV_Vec3 mv)    = VGM.basicUnsafeGrow mv

instance VG.Vector VU.Vector Vec3 where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze            = VG.basicUnsafeFreeze
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw              = VG.basicUnsafeThaw
    {-# INLINE basicLength #-}
    basicLength                  = VG.basicLength
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n         = VG.basicUnsafeSlice i n
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_Vec3 v) = VG.basicUnsafeIndexM v
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_Vec3 mv) = VG.basicUnsafeCopy mv
    {-# INLINE elemseq #-}
    elemseq (V_Vec3 v)           = VG.elemseq v

instance VU.Unbox Vec3


-- | Matrix given by its rows.
data Matrix = Matrix !Vec3 !Vec3 !Vec3
              deriving (Eq, Ord, Show)


-- | Point in @R^3@.
type Point = Vec3


-- | Add two vectors.
(<+>) :: Vec3 -> Vec3 -> Vec3
(<+>) !(Vec3 x1 y1 z1) !(Vec3 x2 y2 z2) = 
    Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
{-# INLINE (<+>) #-}


-- | Subtract two vectors.
(<->) :: Vec3 -> Vec3 -> Vec3
(<->) !(Vec3 x1 y1 z1) !(Vec3 x2 y2 z2) = 
    Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
{-# INLINE (<->) #-}


-- | Vec3 cross product.
(><) :: Vec3 -> Vec3 -> Vec3
(><) !(Vec3 x1 y1 z1) !(Vec3 x2 y2 z2) = 
    Vec3 (y1 * z2 - y2 * z1) (x2 * z1 - x1 * z2) (x1 * y2 - x2 * y1)
{-# INLINE (><) #-}


-- | Scale vector.
(.^) :: Vec3 -> Double -> Vec3
(.^) !(Vec3 x y z) !s = Vec3 (x * s) (y * s) (z * s)
{-# INLINE (.^) #-}


-- | Vec3 dot product.
(.*) :: Vec3 -> Vec3 -> Double
(.*) !(Vec3 x1 y1 z1) !(Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
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
mxv !(Matrix r1 r2 r3) !v = Vec3 (r1 .* v) (r2 .* v) (r3 .* v)
{-# INLINE mxv #-}


-- | Produce matrix with diagonal elements equal to given value.
diag :: Double -> Matrix
diag !d = Matrix (Vec3 d 0 0) (Vec3 0 d 0) (Vec3 0 0 d)
{-# INLINE diag #-}


-- | Transpose vector and multiply it by another vector, producing a
-- matrix.
vxv :: Vec3 -> Vec3 -> Matrix
vxv !(Vec3 v11 v12 v13) !v2 = Matrix (v2 .^ v11) (v2 .^ v12) (v2 .^ v13)
{-# INLINE vxv #-}


-- | Euclidean distance between two points.
distance :: Point -> Point -> Double
distance !v1 !v2 = norm (v1 <-> v2)
{-# INLINE distance #-}


-- | Euclidean norm of vector.
norm :: Vec3 -> Double
norm !(Vec3 x y z) = sqrt (x * x + y * y + z * z)
{-# INLINE norm #-}


-- | Produce unit vector with same direction as the original one.
normalize :: Vec3 -> Vec3
normalize !v = v .^ (1 / norm v)
{-# INLINE normalize #-}


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


-- | Scale vector by -1.
reverse :: Vec3 -> Vec3
reverse !v = v .^ (-1)
{-# INLINE reverse #-}


-- | Add two matrices.
--
-- We could add Applicative instance for Matrix and lift (+) to it.
addM :: Matrix -> Matrix -> Matrix
addM !(Matrix r11 r12 r13) !(Matrix r21 r22 r23) = 
    Matrix (r11 <+> r21) (r12 <+> r22) (r13 <+> r23)
{-# INLINE addM #-}


-- | Build an ortogonal vector parallel to xy plane (rotated CCW
-- around Oz)
horizontalShifter :: Vec3 -> Vec3
horizontalShifter n@(Vec3 x y z) 
    | z == 0 = (Vec3 0 0 1) >< n
    | x == 0 && y == 0 = (Vec3 1 0 0) >< n
    | otherwise = (Vec3 x y 0) >< n


-- | Build cartesian axes from Ox vector so that Oz belongs to plane
-- perpendicular to Oxy (i.e. roll = 0 degree).
buildCartesian :: Vec3 -> (Vec3, Vec3, Vec3)
buildCartesian n = (normalize n, normalize v, normalize w)
    where v = horizontalShifter n
          w = v >< n
