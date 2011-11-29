module DSMC

where

type Point = (Double, Double, Double)

type Vector = (Double, Double, Double)

data Particle = Particle
                {
                  position :: Point,
                  speed :: Vector
                }
                deriving Show

data Domain = Box Double Double Double Double Double Double
              deriving Show

-- 
data Object = Sphere Point Double
            | Plane (Double, Double, Double, Double)

-- Body is a composition of objects
data Body = Primitive Object
          | Union Body Body
          | Intersection Body Body

instance Show Object where
         show (Sphere p d) = "S(" ++ (show p) ++ ";" ++ (show d) ++ ")"
         show (Plane (a, b, c, d)) = "P" ++ (show (a, b, c, d))

instance Show Body where
         show (Primitive a) = show a
         show (Union a b) = "{" ++ (show a) ++ "} ∪ {" ++ (show b) ++ "}"
         show (Intersection a b) = "{" ++ (show a) ++ "} ∩ {" ++ (show b) ++ "}"

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

-- Move particle for t time and update its position
move t (Particle p speed) = (Particle (p <+> (speed *> t)) speed)

inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) v) =
         xmax >= x && x >= xmin &&
         ymax >= y && y >= ymin &&
         zmax >= z && z >= zmin         

clip domain particles = filter (inDomain domain) particles

-- Deflect particle from object.
--
-- Return 2-tuple which contains and new particle with position and
-- velocity updated after hit.
hit :: Particle -> Object -> (Bool, Particle)
hit (Particle (x, y, z) (vx, vy, vz)) (Plane (a, b, c, d)) = 
    let
        t = - (a * x + b * y + c * z + d) / (a * vx + b * vy + c * vz)
        p = (Particle (x, y, z) (vx, vy, vz)) 
    in
        if t > 0 then
           (False, p)
        else
           (True, move (-1 * t) (move t p){speed = (vx, vy, vz) <-> ((n *> 2) *> ((vx, vy, vz) <*> n))})
           where
                n = normalize (a, b, c)
