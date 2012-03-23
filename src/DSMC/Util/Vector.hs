{-| 

Naive vector implementation. 

-}

module DSMC.Util.Vector

where

-- | Vector in R³.
type Vector = (Double, Double, Double)

-- | Point in R³.
type Point = (Double, Double, Double)


-- | Add two vectors.
(x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- | Subtract two vectors.
(x1, y1, z1) <-> (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- | Scale vector.
(x, y, z) *> f = (x * f, y * f, z * f)

-- | Vector dot product.
(x1, y1, z1) <*> (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Vector cross product.
(x1, y1, z1) <×> (x2, y2, z2) = (y1 * z2 - y2 * z1, x2 * z1 - x1 * z2, x1 * y2 - x2 * y1)


-- | Euclidean distance between two points
distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)


-- | Euclidean norm of vector. 
norm :: Vector -> Double
norm v = distance v (0, 0, 0)


-- | Produce unit vector with same direction as the original one.
normalize :: Vector -> Vector
normalize v = v *> (1 / norm v)


-- | Scale vector by -1.
reverse v = v *> (-1)


-- | Build an ortogonal vector parallel to xy plane (rotated CCW
-- around Oz)
horizontalShifter :: Vector -> Vector
horizontalShifter n@(x, y, z) | z == 0 = (0, 0, 1) <×> n
                              | x == 0 && y == 0 = (1, 0, 0) <×> n
                              | otherwise = (x, y, 0) <×> n


-- | Build cartesian axes from Ox vector so that Oz belongs to plane
-- perpendicular to Oxy (i.e. roll = 0°).
buildCartesian :: Vector -> (Vector, Vector, Vector)
buildCartesian n = (normalize n, normalize v, normalize w)
    where v = horizontalShifter n
          w = v <×> n
