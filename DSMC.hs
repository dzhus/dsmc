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
         show (Plane (a, b, c) d) = "P" ++ (show (a, b, c, d))
         show (Sphere p d) = "S(" ++ (show p) ++ ";" ++ (show d) ++ ")"

instance Show Body where
         show (Primitive a) = show a
         show (Union a b) = "{" ++ (show a) ++ "} ∪ {" ++ (show b) ++ "}"
         show (Intersection a b) = "{" ++ (show a) ++ "} ∩ {" ++ (show b) ++ "}"
         show (Complement a) = "∁" ++ (show a)

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

-- Calculate times at which object surface is intersected by
-- particle.
findHits :: Particle -> Object -> Trace
findHits (Particle pos v) (Plane n d)
    | f == 0 = []
    | otherwise = 
        let
            t = (pos <*> n + d) / f
        in
          if t >= 0
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

-- Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vector -> Time -> Particle
reflectSpecular p n t =
    move (-1 * t) p{speed = v <-> (n *> (v <*> n) *> 2)}
    where
      v = speed p

type HitSegment = ((Double, Maybe Vector), (Double, Maybe Vector))

-- Merge two overlapping segments
merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)

-- Overlap two overlapping segments
overlap (a1, b1) (a2, b2) = (max a1 a2, min b1 b2)

-- Reverse both normal vectors of segment
flipNormals ((x, u), (y, v)) = ((x, Vector.reverse `liftM` u), 
                                (y, Vector.reverse `liftM` v))

type Trace = [HitSegment]

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

tryHit :: Particle -> Body -> Trace

tryHit p (Primitive b) =
    findHits p b

tryHit p (Union b1 b2) =
    unite (tryHit p b1) (tryHit p b2)

tryHit p (Intersection b1 b2) =
    intersect (tryHit p b1) (tryHit p b2)

tryHit p (Complement b) =
    complement (tryHit p b)

-- Return particle after possible collision with body during given timespan
hit :: Particle -> Time -> Body -> Particle
hit p dt b = 
    let
        justHit = [(((-dt), Nothing), (0, Nothing))]
        fullTrace = (tryHit p b)
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

