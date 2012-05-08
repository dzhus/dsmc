{-# LANGUAGE ExistentialQuantification #-}

-- | Body compositions for which particle trajectory intersections
-- can be calculated.

module DSMC.Traceables
    ( -- * Traces
      Trace
    -- * Primitive trace operations
    , trace
--    , uniteTraces
--    , intersectTraces
    -- * Traceable bodies
    , Body(..)
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


-- | Trace of a linearly-moving particle on a primitive is the time
-- interval during which particle is considered to be inside the
-- primitive.
--
-- >                       * - particle
-- >                        \
-- >                         \
-- >                          o------------
-- >                      ---/ =           \---
-- >                    -/      =              \-
-- >                   /         =               \
-- >                  (           =  - trace      )
-- >                   \           =             /
-- >                    -\          =          /-
-- >       primitive -  ---\         =     /---
-- >                          --------o----
-- >                                   \
-- >                                    \
-- >                                    _\/
-- >                                      \
--
-- We consider only primitives defined by quadratic or linear
-- surfaces. Thus trace may contain a single segment, or a single
-- half-interval, or two half-intervals. Ends of segments or intervals
-- are calculated by intersecting the trajectory ray of a particle and
-- the surface of the primitive. This may be done by substituting the
-- equation of trajectory @X(t) = X_o + V*t@ into the equation which
-- defines the surface and solving it for @t@.
--
-- For example, since a ray intersects a plane only once, a halfspace
-- primitive defined by this plane results in a half-interval trace of
-- a particle:
--
-- >                                          /
-- >                                         /
-- >                                        /
-- >              *------------------------o=================>
-- >              |                       /                  |
-- >           particle                  /            goes to infinity
-- >                                    /
-- >                                   /
-- >                                  /
-- >                                 / - surface of halfspace
--
-- Normal vectors are calculated for hit points which are not in
-- infinity.
type Trace = Maybe ((Double, Maybe Vector), (Double, Maybe Vector))


-- | Infinity definition for 'RealFloat'.
infinityP :: Double
infinityP = 1 / 0


-- | Negative infinity.
infinityN :: Double
infinityN = -infinityP


data Body = Plane !Vector !Double
          -- ^ Half-space defined by plane with outward normal.
          | Union ![Body]
          -- ^ Union of bodies.
          | Intersection ![Body]
          -- ^ Intersection of bodies.
            deriving Show


trace :: Body -> Particle -> Trace
inside :: Body -> Particle -> Bool


trace (Plane n d) (Particle pos v) =
    let
        nn = normalize n
        f = -(n .* v)
    in
      if f == 0
      then Nothing
      else
          let
              t = (pos .* n + d) / f
          in
            if f > 0
            then Just ((t, Just nn), (infinityP, Nothing))
            else Just ((infinityN, Nothing), (t, Just nn))



inside (Plane n d) (Particle pos _) = (pos .* n + d) < 0

inside (Union bs) p = or $ map (flip inside p) bs

inside (Intersection bs) p = and $ map (flip inside p) bs
