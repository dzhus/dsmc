{-# LANGUAGE ExistentialQuantification #-}

-- | Body compositions for which particle trajectory intersections
-- can be calculated.

module DSMC.Traceables
    ( -- * Traces
      Trace
    , HitPoint(..)
    -- * Primitive trace operations
    , intersectTraces
    , trace
    , hitPoint
    -- * Traceable bodies
    , Body(..)
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst, reverse)

import Data.Functor
import Data.Strict.Maybe
import Data.Strict.Tuple

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
type Trace = Maybe (Pair HitPoint HitPoint)


-- | Time when particle hits the surface, along with normal at hit
-- point. If hit is in infinity, then normal is Nothing.
data HitPoint = HitPoint !Double (Maybe Vector)
                deriving (Eq, Ord, Show)


-- | IEEE positive infinity.
infinityP :: Double
infinityP = 1 / 0


-- | Negative infinity.
infinityN :: Double
infinityN = -infinityP


-- | Intersect two traces.
intersectTraces :: Trace -> Trace -> Trace
intersectTraces tr1 tr2 =
    case tr1 of
      Nothing -> Nothing
      Just (x :!: y) ->
          case tr2 of
            Nothing -> Nothing
            Just (u :!: v) ->
                case (y > u && x < v) of
                  True -> Just $ max x u :!: min y v
                  False -> Nothing
{-# INLINE intersectTraces #-}


-- | CSG body is a recursive composition of primitive objects or other
-- bodies. We require that prior to tracing particles on a body it's
-- converted to sum-of-products form.
data Body = Plane !Vector !Double
          -- ^ Half-space defined by plane with outward normal.
          | Sphere !Vector !Double
          -- ^ Sphere defined by center point and radius.
          | Union !Body !Body
          -- ^ Union of bodies.
          | Intersection !Body !Body
          -- ^ Intersection of bodies.
            deriving Show


-- | Trace a particle on a body.
trace :: Body -> Particle -> Trace

trace (Plane n d) (Particle pos v) =
    let
        nn = normalize n
        f = -(n .* v)
    in
      -- Check if ray is parallel to plane
      if f == 0
      then Nothing
      else
          let
              t = (pos .* n + d) / f
          in
            if f > 0
            then Just ((HitPoint t (Just nn)) :!: (HitPoint infinityP Nothing))
            else Just ((HitPoint infinityN Nothing) :!: (HitPoint t (Just nn)))

trace (Sphere c r) (Particle pos v) =
      let
          d = pos <-> c
          roots = solveq (v .* v) (v .* d * 2) (d .* d - r * r)
          normal u = normalize (u <-> c)
      in
        case roots of
          Nothing -> Nothing
          Just (t1 :!: t2) -> Just $ 
                           (HitPoint t1 (Just $ normal $ moveBy pos v t1) :!:
                            HitPoint t2 (Just $ normal $ moveBy pos v t2))

trace (Intersection b1 b2) p =
    intersectTraces tr1 tr2
        where
          tr1 = trace b1 p
          tr2 = trace b2 p


-- | Get first hit point of particle on a body surface.
hitPoint :: Body -> Particle -> Maybe HitPoint
hitPoint b p = fst <$> trace b p
