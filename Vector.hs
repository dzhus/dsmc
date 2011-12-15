module Vector

where

type Point = (Double, Double, Double)

type Vector = (Double, Double, Double)

-- Vector operations
(x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
(x1, y1, z1) <-> (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
(x, y, z) *> f = (x * f, y * f, z * f)
(x1, y1, z1) <*> (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

(x1, y1, z1) <×> (x2, y2, z2) = (y1 * z2 - y2 * z1, x2 * z1 - x1 * z2, x1 * y2 - x2 * y1)

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)

norm :: Vector -> Double
norm v = distance v (0, 0, 0)

normalize :: Vector -> Vector
normalize v = v *> (1 / norm v)

reverse v = v *> (-1)

-- Build an ortogonal vector parallel to xy plane (rotated CW around
-- Oz)
horizontalShifter :: Vector -> Vector
horizontalShifter (nx, ny, nz) = if nx /= 0
                                 then (-ny * y / nx, y, 0)
                                 else (-ny, 0, 0)
                                     where y = if nx >= 0 then -1 else 1

buildCartesian :: Vector -> (Vector, Vector, Vector)
buildCartesian v = (normalize v, normalize x, normalize y)
    where x = horizontalShifter v
          y = x <×> v
