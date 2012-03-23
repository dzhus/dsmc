{-| 

Naive vector implementation. 

-}

module DSMC.Util.Vector

where

-- | Vector in R³.
data Vector = Vector !Double !Double !Double
              deriving (Eq, Ord, Show)

-- | Point in R³.
type Point = Vector


instance Num Vector where
    -- | Add two vectors.
    (+) (Vector x1 y1 z1) (Vector x2 y2 z2) = 
        Vector (x1 + x2) (y1 + y2) (z1 + z2)
    -- | Subtract two vectors.
    (-) (Vector x1 y1 z1) (Vector x2 y2 z2) = 
        Vector (x1 - x2) (y1 - y2) (z1 - z2)
    -- | Vector cross product.
    (*) (Vector x1 y1 z1) (Vector x2 y2 z2) = 
        Vector (y1 * z2 - y2 * z1) (x2 * z1 - x1 * z2) (x1 * y2 - x2 * y1)


-- | Scale vector.
scale :: Vector -> Double -> Vector
scale (Vector x y z) s = Vector (x * s) (y * s) (z * s)

-- | Infix shortcut for 'scale'.
(.^) :: Vector -> Double -> Vector
(.^) = scale

-- | Vector dot product.
dotP :: Vector -> Vector -> Double
dotP (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Infix shortcut for *dot* product.
(.*) :: Vector -> Vector -> Double
(.*) = dotP


-- | Euclidean distance between two points.
distance :: Point -> Point -> Double
distance v1 v2 = norm (v1 - v2)


-- | Euclidean norm of vector.
norm :: Vector -> Double
norm (Vector x y z) = sqrt (x * x + y * y + z * z)


-- | Produce unit vector with same direction as the original one.
normalize :: Vector -> Vector
normalize v = scale v (1 / norm v)


-- | Scale vector by -1.
reverse v = scale v (-1)


-- | Build an ortogonal vector parallel to xy plane (rotated CCW
-- around Oz)
horizontalShifter :: Vector -> Vector
horizontalShifter n@(Vector x y z) 
    | z == 0 = (Vector 0 0 1) * n
    | x == 0 && y == 0 = (Vector 1 0 0) * n
    | otherwise = (Vector x y 0) * n


-- | Build cartesian axes from Ox vector so that Oz belongs to plane
-- perpendicular to Oxy (i.e. roll = 0°).
buildCartesian :: Vector -> (Vector, Vector, Vector)
buildCartesian n = (normalize n, normalize v, normalize w)
    where v = horizontalShifter n
          w = v * n
