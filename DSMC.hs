module DSMC

where

type Point = (Double, Double, Double)

type Vector = (Double, Double, Double)

type Time = Double

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

-- Normalized normal vector to surface at point
normal :: Object -> Point -> Vector
normal (Plane (a, b, c, d)) _ = normalize (a, b, c)

-- Calculate time until object is hit by particle. If not positive, then
-- particle is inside the object.
timeToHit :: Particle -> Object -> Time
timeToHit (Particle (x, y, z) (vx, vy, vz)) (Plane (a, b, c, d)) =
    - (a * x + b * y + c * z + d) / (a * vx + b * vy + c * vz)

-- Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vector -> Time -> Particle
reflectSpecular p n t =
    move (-1 * t) (move t p){speed = v <-> (n *> (v <*> n) *> 2)}
    where
      v = speed p

-- Return Maybe 2-tuple containing time until with hit body and closest
-- object to hit. If no objects are hit so far, return Nothing.
hit :: Particle -> Body -> Maybe (Time, Object)
hit p (Primitive o) = 
    let
        t = timeToHit p o 
    in
      if t > 0
      then Nothing
      else Just (t, o)

hit p (Intersection b1 b2) =
    let
        r1 = hit p b1
        r2 = hit p b2
    in
      case (r1, r2) of
        (Just (t1, o1), Just (t2, o2)) -> if abs t1 < abs t2
                                          then r1
                                          else r2
        _ -> Nothing
                                              
           