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
    , cone
    , union
    , intersection
    , complement
    -- * Infinity definitions
    , infinityP
    , infinityN
    )

where

import Prelude hiding (reverse)

import Data.Functor
import Data.List (foldl')

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


-- | Infinity definition for 'RealFloat'.
infinityP :: Double
infinityP = 1 / 0

-- | Negative infinity.
infinityN :: Double
infinityN = -infinityP


-- | Unite two traces.
uniteTraces :: Trace -> Trace -> Trace
uniteTraces tr1 (hs:t2) =
    uniteTraces (unite' tr1 hs) t2
    where
      -- Merge two *overlapping* 'HitSegment's
      merge (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)
      {-# INLINE merge #-}
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
    foldl' uniteTraces [] (map (\hs -> intersect' tr1 hs) tr2)
    where
      -- Overlap two overlapping segments.
      --
      -- If overlap results in a single point, then preserve real vectors
      -- over Nothing.
      overlap (a1, b1) (a2, b2) =
          collapsePoint (max a1 a2, min b1 b2)
          where
            collapsePoint (f@(x, u), s@(y, v)) = 
                if x == y
                then ((x, max u v), (y, max u v))
                else (f, s)
      {-# INLINE overlap #-}
      intersect' (hs1@(a1, b1):t1) hs2@(a, b)
          | b < a1 = []
          | a > b1 = intersect' t1 hs2
          | otherwise = (overlap hs1 hs2):(intersect' t1 (min b b1, max b b1))
      intersect' [] _ = []


-- | Complement to trace (normals flipped) in R³.
complementTraces :: Bool -> Trace -> Trace
complementTraces doFlip (x:xs) =
    start ++ (complementTraces' (snd x) xs)
    where
      flipNormals s@((a, u), (b, v)) = if doFlip then ((a, reverse <$> u),
                                                       (b, reverse <$> v))
                                       else s
      {-# INLINE flipNormals #-}
      start = if (isInfinite (fst (fst x)))
              then []
              else [flipNormals ((infinityN, Nothing), fst x)]
      complementTraces' c (h:tr) = (flipNormals (c, fst h)):(complementTraces' (snd h) tr)
      complementTraces' c [] = if (isInfinite (fst c))
                         then []
                         else [flipNormals (c, (infinityP, Nothing))]
complementTraces _ [] = [((infinityN, Nothing), (infinityP, Nothing))]


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
      nn = normalize n
      thisTrace (Particle pos v) =
          let
            f = -(n .* v)
          in
            if f == 0
            then []
            else
                let
                    t = (pos .* n + d) / f
                in
                  if f > 0
                  then [((t, Just nn), (infinityP, Nothing))]
                  else [((infinityN, Nothing), (t, Just nn))]
      thisInside (Particle pos _) = (pos .* n + d) < 0


-- | Ball defined by center point and radius.
sphere :: Point -> Double -> Body
sphere c r =
    Body thisTrace thisInside
    where
      thisTrace p@(Particle pos v) =
          let
              d = pos <-> c
              roots = solveq ((v .* v), (v .* d * 2), (d .* d - r * r))
              normal u = normalize (u <-> c)
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
      r2 = r * r
      nn = normalize n
      thisTrace p@(Particle pos v) =
          let
              d = (pos <-> c) >< n
              e = v >< n
              roots = solveq ((e .* e), (d .* e * 2), (d .* d - r2))
              normal u = normalize $ h <-> (nn .^ (h .* nn))
                  where h = u <-> c
          in
            traceQuadratic p roots normal
      thisInside (Particle pos _) =
          let
              d = (pos <-> c)
              s = nn .^ (d .* nn)
              rn = d <-> s
          in
            (norm rn) < r


cone :: Vector
     -- ^ Cone axis direction (from vertex to inside)
     -> Point 
     -- ^ Cone vertex
     -> Double
     -- ^ Angle between axis and outer edge, in radians.
     -> Body
cone n c a = 
    intersection [plane (reverse n) (-(norm c)), Body thisTrace thisInside]
    where
      nn = normalize n
      a' = cos a
      gamma = diag (-a' * a')
      thisTrace p@(Particle pos v) =
          let
              delta = pos <-> c
              m = addM (nn `vxv` nn) gamma
              c2 = dotM v     v     m
              c1 = dotM v     delta m
              c0 = dotM delta delta m
              roots = solveq (c2, 2 * c1, c0)
              normal u = normalize $ nx .^ (1 / ta)  <-> ny .^ ta
                  where h = u <-> c
                        -- Component of h parallel to cone axis
                        ny' = nn .^ (nn .* h)
                        ny = normalize ny'
                        -- Perpendicular component
                        nx = normalize $ h <-> ny'
                        ta = tan a
              tr = traceQuadratic p roots normal
          in
            if (v .* nn) / (norm v) < a' then tr else complementTraces False tr
      thisInside _ = True


-- | Union of bodies.
union :: [Body] -> Body
union bodies =
    Body thisTrace thisInside
    where
      thisTrace p =
          let
              t:ts = map (flip trace p) bodies
          in
            foldl' uniteTraces t ts
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
            foldl' intersectTraces t ts
      thisInside p = or (map (flip inside p) bodies)

-- | Complement to body in universe R³ (normals flipped).
complement :: Body -> Body
complement body =
    Body thisTrace thisInside
    where
      thisTrace p = complementTraces True $ flip trace p body
      thisInside p = not $ inside body p
