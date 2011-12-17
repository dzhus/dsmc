module DSMC

where

import Control.Monad
import Data.Maybe
import Debug.Trace

import Vector

type Time = Double

data Particle = Particle
                {
                  position :: Point,
                  speed :: Vector
                }
                deriving Show

data Domain = Box Double Double Double Double Double Double
              deriving Show

-- Simple geometrical object
data Object = Sphere Point Double
            | Plane Vector Double
            deriving (Eq, Ord)

-- Body is a composition of objects
data Body = Primitive Object
          | Union [Body]
          | Intersection [Body]
          | Complement Body
          deriving Show

instance Show Object where
         show (Plane (a, b, c) d) = "P" ++ (show (a, b, c, d))
         show (Sphere p d) = "S(" ++ (show p) ++ ";" ++ (show d) ++ ")"

-- Move particle for t time and update its position
move t (Particle p speed) = (Particle (p <+> (speed *> t)) speed)

inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) v) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin         

clipDomain domain particles = filter (inDomain domain) particles

-- Normalized normal vector to surface at point
normal :: Object -> Point -> Vector
normal (Plane n d) _ = normalize n
normal (Sphere c r) p = normalize (p <-> c)

infinityP = 1 / 0
infinityN = -1 / 0

-- Solve quadratic equation
solveq :: (Double, Double, Double) -> [Double]
solveq (a, b, c)
    | (d < 0) = []
    | (d > 0) = [(- b - sqrt d) / (2 * a), (- b + sqrt d) / (2 * a)]
    | otherwise = [- b / (2 * a)]
    where
      d = b * b - 4 * a * c

type HitSegment = ((Double, Maybe Vector), (Double, Maybe Vector))
type Trace = [HitSegment]

-- Calculate times at which object surface is intersected by particle.
-- Return a list of ((t1, n1), (t2, n2)), where t1 and t2 are
findHits :: Particle -> Object -> Trace
findHits (Particle pos v) (Plane n d)
    | f == 0 = []
    | otherwise = 
        let
            t = (pos <*> n + d) / f
        in
          if f > 0
          then [((t, Just nn), (infinityP, Nothing))]
          else [((infinityN, Nothing), (t, Just nn))]
    where
      f = -(n <*> v)
      nn = normalize n

findHits p@(Particle pos v) s@(Sphere c r) =
    let
        d = pos <-> c
        roots = solveq ((v <*> v), (v <*> d * 2), (d <*> d - r ^ 2))
    in
      case roots of
        [t1, t2] ->
            let
                p1 = move t1 p
                p2 = move t2 p
            in
              [((t1, Just (normal s (position p1))), (t2, Just (normal s (position p2))))]
        _ -> []

inside :: Particle -> Object -> Bool
inside (Particle pos v) (Plane n d) = (pos <*> n + d) < 0
inside (Particle pos v) (Sphere c r) = (distance pos c) < r

-- Merge two overlapping segments
merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)

-- Overlap two overlapping segments
--
-- If overlap results in a single point, then preserve real vectors
-- over Nothing.
overlap (a1, b1) (a2, b2) = collapsePoint (max a1 a2, min b1 b2)
                            where
                              collapsePoint (f@(x, u), s@(y, v)) = if x == y
                                                                   then ((x, max u v), (y, max u v))
                                                                   else (f, s)

-- Reverse both normal vectors of segment
flipNormals ((x, u), (y, v)) = ((x, Vector.reverse `liftM` u), 
                                (y, Vector.reverse `liftM` v))

unite :: Trace -> Trace -> Trace
unite hsl1 (hs:t2) =
    unite (unite' hsl1 hs) t2
    where
      unite' (hs1@(a1, b1):t1) hs2@(a, b)
          | b < a1 = hs2:hs1:t1
          | a > b1 = hs1:(unite' t1 hs2)
          | otherwise = unite' t1 (merge hs1 hs2)
      unite' [] hs2 = [hs2]
unite hsl1 [] = hsl1

intersect :: Trace -> Trace -> Trace
intersect tr1 tr2 =
    foldl unite [] (map (\hs -> intersect' tr1 hs) tr2)
    where
      intersect' (hs1@(a1, b1):t1) hs2@(a, b)
          | b < a1 = []
          | a > b1 = intersect' t1 hs2
          | otherwise = (overlap hs1 hs2):(intersect' t1 (min b b1, max b b1))
      intersect' [] hs2 = []

complement :: Trace -> Trace
complement (x:xs) = 
    start ++ (complement' (snd x) xs)
    where
      start = if (isInfinite (fst (fst x)))
              then []
              else [flipNormals ((infinityN, Nothing), fst x)]
      complement' c (h:tr) = (flipNormals (c, fst h)):(complement' (snd h) tr)
      complement' c [] = if (isInfinite (fst c))
                         then []
                         else [flipNormals (c, (infinityP, Nothing))]
complement [] = [((infinityN, Nothing), (infinityP, Nothing))]

-- Find possible collisions of particle with respect to body structure
traceParticle :: Particle -> Body -> Trace

traceParticle p (Primitive b) =
    findHits p b

traceParticle p (Union bodies) =
    let
        t:ts = map (traceParticle p) bodies
    in
      foldl unite t ts

traceParticle p (Intersection bodies) =
    let
        t:ts = map (traceParticle p) bodies
    in
      foldl intersect t ts

traceParticle p (Complement b) =
    complement (traceParticle p b)

-- Find if particle is inside the body
insideBody :: Particle -> Body -> Bool

insideBody p (Primitive o) =
    inside p o

insideBody p (Union bodies) =
    or (map (insideBody p) bodies)

insideBody p (Intersection bodies) =
    and (map (insideBody p) bodies)

insideBody p (Complement b) =
    not (insideBody p b)

-- Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vector -> Time -> Particle
reflectSpecular p n t =
    move (-1 * t) p{speed = v <-> (n *> (v <*> n) *> 2)}
    where
      v = speed p

-- Particle after possible collision with body during timestep
hit :: Time -> Body -> Particle -> Particle
hit dt b p = 
    let
        justHit = [(((-dt), Nothing), (0, Nothing))]
        fullTrace = (traceParticle p b)
        trace = intersect fullTrace justHit
    in
      if null trace
      then p
      else
          let
              hitPoint = fst (head trace)
              t = fst hitPoint
              n = snd hitPoint
              particleAtHit = move t p
          in
            reflectSpecular particleAtHit (fromJust n) t

clipBody body particles = filter (\p -> not (insideBody p body)) particles
