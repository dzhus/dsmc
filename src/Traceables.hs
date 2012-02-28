{-# LANGUAGE ExistentialQuantification #-}

-- | Body compositions for which particle trajectory intersections
-- can be calculated.

module Traceables
    ( -- * Traces
      HitSegment
    , Trace
    -- * Primitive trace operations
    , unite
    , intersect
    , complement
    -- * Traceable bodies
    , Body(..)
    -- ** Operations on traceables
    , trace
    , inside
    )

where

import Data.Functor

import Particles
import Util
import Vector


-- | HitSegment of a linearly-moving particle on a body is (one of)
-- the time interval during which particle is considered to be inside
-- the body.
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
-- >  traceable body →  ---\         =     /---
-- >                          --------o----
-- >                                   \
-- >                                    \
-- >                                    _\/
-- >                                      \
--
-- Normal vectors are calculated for hit points which are not in
-- infinity.
type HitSegment = ((Double, Maybe Vector), (Double, Maybe Vector))


-- | Particle may enter and leave body several times (in case of
-- higher-than-2 order surfaces or complex bodies), thus its full
-- trace may include several 'HitSegment's.
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


data Body = Plane Vector Double
          -- ^ Half-space defined by plane with outward normal.
          | Sphere Point Double
          -- ^ Ball defined by center point and radius.
          | Cylinder Vector Point Double
          -- ^ Construct cylinder with axis vector, point on axis and
          -- radius.
          | Union [Body]
          -- ^ Union of bodies.
          | Intersection [Body]
          -- ^ Intersection of bodies.
          | Complement Body


instance Show Body where
    show (Plane (a, b, c) d) = "P" ++ (show (a, b, c, d))
    show (Sphere p d) = "S(" ++ (show p) ++ ";" ++ (show d) ++ ")"


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

-- | Calculate particle's trace on body.
trace :: Particle -> Body -> Trace
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


trace p@(Particle pos v) s@(Sphere c r) =
    let
        d = pos <-> c
        roots = solveq ((v <*> v), (v <*> d * 2), (d <*> d - r ^ 2))
        normal p = normalize (p <-> c)
    in
      traceQuadratic p roots normal


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


trace p (Union bodies) =
    let
        t:ts = map (trace p) bodies
    in
      foldl unite t ts


trace p (Intersection bodies) =
    let
        t:ts = map (trace p) bodies
    in
      foldl intersect t ts


trace p (Complement b) = complement $ trace p b

-- | Body membership predicate.
inside :: Particle -> Body -> Bool

inside (Particle pos v) (Plane n d) = (pos <*> n + d) < 0


inside (Particle pos v) (Sphere c r) = (distance pos c) < r


inside p (Union bodies) = and (map (inside p) bodies)


inside p (Intersection bodies) = or (map (inside p) bodies)


inside p (Complement b) = not $ inside p b
