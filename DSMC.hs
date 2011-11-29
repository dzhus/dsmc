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
            | Plane Vector Double

-- Body is a composition of objects
data Body = Primitive Object
          | Union Body Body
          | Intersection Body Body

instance Show Object where
         show (Sphere p d) = "S(" ++ (show p) ++ ";" ++ (show d) ++ ")"
         show (Plane (a, b, c) d) = "P" ++ (show (a, b, c, d))

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
normal (Plane n d) _ = normalize n

-- Calculate time until object is hit by particle. If not positive, then
-- particle is inside the object.
timeToHit :: Particle -> Object -> Time
timeToHit (Particle pos v) (Plane n d) =
    (pos <*> n + d) / abs(n <*> v)

-- Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vector -> Time -> Particle
reflectSpecular p n t =
    move (-1 * t) p{speed = v <-> (n *> (v <*> n) *> 2)}
    where
      v = speed p

-- Return Maybe 2-tuple containing time until with hit body and closest
-- object to hit. If no objects are hit so far, return Nothing.
tryHit :: Particle -> Body -> Maybe (Time, Object)
tryHit p (Primitive obj) = 
    let
        t = timeToHit p obj 
    in
      if t > 0
      then Nothing
      else Just (t, obj)

tryHit p (Intersection b1 b2) =
    let
        r1 = tryHit p b1
        r2 = tryHit p b2
    in
      case (r1, r2) of
        -- When last object is hit, the whole intersection is hit
        (Just (t1, obj1), Just (t2, obj2)) -> if abs t1 < abs t2
                                              then r1
                                              else r2
        _ -> Nothing

tryHit p (Union b1 b2) =
    let
        r1 = tryHit p b1
        r2 = tryHit p b2
    in
      case (r1, r2) of
        (Nothing, Nothing) -> Nothing
        (r1, Nothing) -> r1
        (Nothing, r2) -> r2
        -- When first object is hit, the union is hit
        (Just (t1, obj1), Just (t2, obj2)) -> if abs t1 > abs t2
                                              then r1
                                              else r2

-- Return particle after possible collision with body
hit :: Particle -> Body -> Particle
hit p b = 
    let
        r = tryHit p b
    in
      case r of
        Just (t, obj) -> reflectSpecular particleAtHit (normal obj (position particleAtHit)) t
                         where particleAtHit = move t p
        Nothing -> p
