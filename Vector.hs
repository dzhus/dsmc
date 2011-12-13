module Vector

where

type Point = (Double, Double, Double)

type Vector = (Double, Double, Double)

-- Vector operations
(x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
(x1, y1, z1) <-> (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
(x, y, z) *> f = (x * f, y * f, z * f)
(x1, y1, z1) <*> (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)

norm :: Vector -> Double
norm v = distance v (0, 0, 0)

normalize :: Vector -> Vector
normalize v = v *> (1 / norm v)

reverse v = v *> (-1)