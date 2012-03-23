{-# LANGUAGE ExistentialQuantification #-}

-- | Body compositions for which particle trajectory intersections
-- can be calculated.

module DSMC.Traceables
    ( -- * Traces
      HitSegment
    , Trace
    -- * Primitive trace operations
    , uniteTraces
    , intersectTraces
    , complementTraces
    -- * Traceable bodies
    , Body(..)
    -- * Body constructors
    , plane
    , sphere
    , cylinder
    , union
    , intersection
    , complement
    )

where

import Prelude hiding (reverse)

import Data.Functor

import DSMC.Particles
import DSMC.Types
import DSMC.Util
import DSMC.Util.Vector


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
flipNormals ((x, u), (y, v)) = ((x, reverse <$> u),
                                (y, reverse <$> v))


-- | Unite two traces.
uniteTraces :: Trace -> Trace -> Trace
uniteTraces tr1 (hs:t2) =
    uniteTraces (unite' tr1 hs) t2
    where
      -- Merge two *overlapping* 'HitSegment's
      merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)
      -- Unite trace with single 'HitSegment'
      unite' (hs1@(a1, b1):t1) hs2@(a, b)
          | b < a1 = hs2:hs1:t1
          | a > b1 = hs1:(unite' t1 hs2)
          | otherwise = unite' t1 (merge hs1 hs2)
      unite' [] hs2 = [hs2]
uniteTraces tr1 [] = tr1

-- | Intersect two traces.
intersectTraces :: Trace -> Trace -> Trace
intersectTraces tr1 tr2 =
    foldl uniteTraces [] (map (\hs -> intersect' tr1 hs) tr2)
    where
      intersect' (hs1@(a1, b1):t1) hs2@(a, b)
          | b < a1 = []
          | a > b1 = intersect' t1 hs2
          | otherwise = (overlap hs1 hs2):(intersect' t1 (min b b1, max b b1))
      intersect' [] _ = []


-- | Complement to trace (normals flipped) in R³.
complementTraces :: Trace -> Trace
complementTraces (x:xs) =
    start ++ (complementTraces' (snd x) xs)
    where
      start = if (isInfinite (fst (fst x)))
              then []
              else [flipNormals ((infinityN, Nothing), fst x)]
      complementTraces' c (h:tr) = (flipNormals (c, fst h)):(complementTraces' (snd h) tr)
      complementTraces' c [] = if (isInfinite (fst c))
                         then []
                         else [flipNormals (c, (infinityP, Nothing))]
complementTraces [] = [((infinityN, Nothing), (infinityP, Nothing))]


-- | Body for which particle trace and membership can be calculated.
data Body = Body { trace :: Particle -> Trace
                 -- ^ Calculate trace of particle on the body.
                 , inside :: Particle -> Bool
                 -- ^ Check if particle is inside the body.
                 }


-- | Trace particle on quadratic surface.
traceQuadratic :: Particle               -- ^ Particle being traced
               -> Maybe (Double, Double) -- ^ Roots of intersection
                                         -- equation for particle
                                         -- trajectory and surface
               -> (Point -> Vector)      -- ^ Normal of surface at
                                         -- point
               -> Trace                  -- ^ Da trace
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


-- | Half-space defined by plane with outward normal.
plane :: Vector -> Double -> Body
plane n d =
    Body thisTrace thisInside
    where
      thisTrace (Particle pos v) =
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
      thisInside (Particle pos _) = (pos <*> n + d) < 0


-- | Ball defined by center point and radius.
sphere :: Point -> Double -> Body
sphere c r =
    Body thisTrace thisInside
    where
      thisTrace p@(Particle pos v) =
          let
              d = pos <-> c
              roots = solveq ((v <*> v), (v <*> d * 2), (d <*> d - r ^ 2))
              normal p = normalize (p <-> c)
          in
            traceQuadratic p roots normal
      thisInside (Particle pos _) = (distance pos c) < r

-- | Infinite cylinder defined by vector collinear to axis, point on
-- axis and radius.
--
-- TODO: 'inside' is not implemented (always False for now).
cylinder :: Vector -> Point -> Double -> Body
cylinder n c r =
    Body thisTrace thisInside
    where
      thisTrace p@(Particle pos v) =
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
      thisInside _ = False

-- | Union of bodies.
union :: [Body] -> Body
union bodies =
    Body thisTrace thisInside
    where
      thisTrace p =
          let
              t:ts = map (flip trace p) bodies
          in
            foldl uniteTraces t ts
      thisInside p = and (map (flip inside p) bodies)

-- | Intersection of bodies.
intersection :: [Body] -> Body
intersection bodies =
    Body thisTrace thisInside
    where
      thisTrace p =
          let
              t:ts = map (flip trace p) bodies
          in
            foldl intersectTraces t ts
      thisInside p = or (map (flip inside p) bodies)

-- | Complement to body in universe R³ (normals flipped).
complement :: Body -> Body
complement body =
    Body thisTrace thisInside
    where
      thisTrace p = complementTraces $ flip trace p body
      thisInside p = not $ inside body p
