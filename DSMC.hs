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
            deriving (Eq, Ord)

-- Body is a composition of objects
data Body = Primitive Object
          | Union Body Body
          | Intersection Body Body
          | Complement Body

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
normal (Sphere c r) p = normalize (p <-> c)

-- Solve quadratic equation
solveq :: (Double, Double, Double) -> [Double]
solveq (a, b, c)
    | (d < 0) = []
    | (d > 0) = [(- b - sqrt d) / (2 * a), (- b + sqrt d) / (2 * a)]
    | otherwise = [- b / (2 * a)]
    where
      d = b * b - 4 * a * c

-- Calculate time until object is hit by particle. If not positive,
-- then particle is inside the object. Moving particle for this time
-- results in hit point. If particle will never hit the object,
-- Nothing is returned.
timeToHit :: Particle -> Object -> Maybe Time
timeToHit (Particle pos v) (Plane n d)
    | f == 0 = Nothing
    | otherwise = Just ((pos <*> n + d) / f)
    where
      f = abs(n <*> v)

timeToHit (Particle pos v) (Sphere c r) =
    if (distance pos c) < r
    then
        let
            d = pos <-> c
            roots = solveq ((v <*> v), (v <*> d * 2), (d <*> d - r ^ 2))
        in
          if null roots
          then Nothing
          else Just (minimum roots)
    else Nothing

-- Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vector -> Time -> Particle
reflectSpecular p n t =
    move (-1 * t) p{speed = v <-> (n *> (v <*> n) *> 2)}
    where
      v = speed p

-- We have to preserve time until hit even if no hit occured to
-- properly process object complements
data HitResult = BeenHit Time Object
               | ToHit Time Object
               | NeverHit
               deriving (Eq, Ord, Show)

-- Return HitResult containing Hit with time since hit with body and
-- object which was hit first. Return NoHit if no objects are hit so
-- far.
tryHit :: Particle -> Body -> HitResult
tryHit p (Primitive obj) = 
    let
        mt = timeToHit p obj
    in
      case mt of
        (Just t) ->  if t > 0
                     then ToHit t obj
                     else BeenHit (abs t) obj
        Nothing -> NeverHit

tryHit p (Complement body) =
    let
        hr = tryHit p body
    in
      case hr of 
        (ToHit t obj) -> BeenHit t obj
        (BeenHit t obj) -> (ToHit t obj)
        _ -> hr

tryHit p (Intersection b1 b2) =
    let
        hr1 = tryHit p b1
        hr2 = tryHit p b2
    in
      case (hr1, hr2) of
        (BeenHit t1 obj1, BeenHit t2 obj2) -> min hr1 hr2
        (NeverHit, NeverHit) -> NeverHit
        (NeverHit, _) -> hr2
        (_, NeverHit) -> hr1
        _ -> max hr1 hr2

tryHit p (Union b1 b2) =
    let
        hr1 = tryHit p b1
        hr2 = tryHit p b2
    in
      case (hr1, hr2) of
        (BeenHit _ _, BeenHit _ _) -> max hr1 hr2
        (BeenHit _ _, _) -> hr1
        (_, BeenHit _ _) -> hr2
        (ToHit t1 obj1, ToHit t2 obj2) -> max hr1 hr2
        (NeverHit, NeverHit) -> NeverHit
        (NeverHit, _) -> hr2
        (_, NeverHit) -> hr1

-- Return particle after possible collision with body
hit :: Particle -> Body -> Particle
hit p b = 
    let
        hr = tryHit p b
    in
      case hr of
        (BeenHit th obj) -> 
            let
                t = (-th)
                particleAtHit = move t p
            in
              reflectSpecular particleAtHit (normal obj (position particleAtHit)) t
        _ -> p
