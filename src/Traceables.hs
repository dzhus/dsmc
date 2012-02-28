{-# LANGUAGE ExistentialQuantification #-}

-- | Object compositions for which particle trajectory intersections
-- can be calculated.

module Traceables
    ( -- * Traces
      HitSegment
    , Trace
      -- * Trace operations
    , unite
    , intersect
    , complement
      -- ** Traceable objects
    , Traceable(..)
    , Plane(..)
    , Sphere(..)
    , Cylinder(..)
    , Intersection(..)
    , Union(..)
    , Complement(..)
    )

where

import Data.Functor

import Particles
import Util
import Vector


-- | HitSegment of a linearly-moving particle on traceable is a time
-- interval during which particle is considered to be inside the
-- traceable.
--
-- >                       * ← particle
-- >                        \
-- >                         \
-- >                          o------------
-- >                      ---/ =           \---
-- >                    -/      =              \-
-- >                   /         =               \
-- >                  (           =  ← HitSegment )
-- >                   \           =             /
-- >                    -\          =          /-
-- >  traceable object →  ---\       =     /---
-- >                          --------o----
-- >                                   \
-- >                                    \
-- >                                    _\/
-- >                                      \
--
-- Normal vectors are calculated for hit points which are not in
-- infinity.
type HitSegment = ((Double, Maybe Vector), (Double, Maybe Vector))


-- | Particle may enter and leave object several times (in case of
-- higher-than-2 order surfaces), thus its full trace may include
-- several 'HitSegment's.
type Trace = [HitSegment]


-- | Merge two overlapping segments.
merge :: HitSegment -> HitSegment -> HitSegment
merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)


-- | Overlap two overlapping segments.
--
-- If overlap results in a single point, then preserve real vectors
-- over Nothing.
overlap :: HitSegment -> HitSegment -> HitSegment
overlap (a1, b1) (a2, b2) =
    collapsePoint (max a1 a2, min b1 b2)
        where
          collapsePoint (f@(x, u), s@(y, v)) = if x == y
                                               then ((x, max u v), (y, max u v))
                                               else (f, s)


-- | Reverse both normal vectors of segment.
flipNormals :: HitSegment -> HitSegment
flipNormals ((x, u), (y, v)) = ((x, Vector.reverse <$> u),
                                (y, Vector.reverse <$> v))


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


-- | Complement to trace (normals flipped) in ℝ³.
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


class Traceable a where
    -- | Calculate trace of particle on object.
    trace :: Particle -> a -> Trace
    -- | Check if particle is inside the object.
    inside :: Particle -> a -> Bool

-- | Half-space defined by plane with outward normal.
data Plane =
    -- | Construct plane with a normal vector and distance from
    -- origin.
    Plane Vector Double 

instance Show Plane where
    show (Plane (a, b, c) d) = "P" ++ (show (a, b, c, d))

instance Traceable Plane where
    trace (Particle pos v) (Plane n d) =
        let
          f = -(n <*> v)
          nn = normalize n
        in
          if f == 0
          then []
          else
              let
                  t = (pos <*> n + d) / f
              in
                if f > 0
                then [((t, Just nn), (infinityP, Nothing))]
                else [((infinityN, Nothing), (t, Just nn))]
    inside (Particle pos v) (Plane n d) = 
        (pos <*> n + d) < 0


-- | Trace particle on quadratic surface.
traceQuadratic :: Particle               -- ^ Particle being traced
               -> Maybe (Double, Double) -- ^ Roots of intersection
                                         -- equation for particle
                                         -- trajectory and surface
               -> (Point -> Vector)      -- ^ Normal of surface at
                                         -- point
               -> Trace
traceQuadratic particle roots normal =
    case roots of
      Just (t1, t2) ->
          let
              p1 = move t1 particle
              p2 = move t2 particle
          in
            [((t1, Just (normal (position p1))),
              (t2, Just (normal (position p2))))]
      Nothing -> []


data Sphere =
    -- | Construct sphere with a center point and radius.
    Sphere Point Double 

instance Show Sphere where
    show (Sphere p d) = "S(" ++ (show p) ++ ";" ++ (show d) ++ ")"

instance Traceable Sphere where
    trace p@(Particle pos v) s@(Sphere c r) =
        let
            d = pos <-> c
            roots = solveq ((v <*> v), (v <*> d * 2), (d <*> d - r ^ 2))
            normal p = normalize (p <-> c)
        in
          traceQuadratic p roots normal
    inside (Particle pos v) (Sphere c r) =
        (distance pos c) < r

-- | Infinite cylinder.
data Cylinder =
    -- | Construct cylinder with axis vector, point on axis and
    -- radius.
    Cylinder Vector Point Double

instance Traceable Cylinder where
    trace p@(Particle pos v) cyl@(Cylinder n c r) =
        let
            d = (pos <-> c) <×> n
            e = v <×> n
            roots = solveq ((e <*> e), (d <*> e * 2), (d <*> d - r ^ 2))
            normal p = nor
                where nor = normalize (h <-> (nn *> (h <*> nn)))
                      h = p <-> c
                      nn = normalize n
        in
          traceQuadratic p roots normal


-- | Union of traceables.
data Union = forall b. Traceable b => Union [b]

instance Traceable Union where
    trace p (Union bodies) =
        let
            t:ts = map (trace p) bodies
        in
          foldl unite t ts
    inside p (Union bodies) =
        and (map (inside p) bodies)


-- | Intersection of traceables.
data Intersection = forall b. Traceable b => Intersection [b]

instance Traceable Intersection where
    trace p (Intersection bodies) =
        let
            t:ts = map (trace p) bodies
        in
          foldl intersect t ts
    inside p (Intersection bodies) =
        or (map (inside p) bodies)


-- | Complement to traceable in universe ℝ³.
data Complement = forall b. Traceable b => Complement b

instance Traceable Complement where
    trace p (Complement b) =
        complement $ trace p b
    inside p (Complement b) =
        not $ inside p b
