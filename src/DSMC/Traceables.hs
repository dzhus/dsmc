{-# LANGUAGE BangPatterns #-}

-- | Body compositions for which particle trajectory intersections
-- can be calculated.

module DSMC.Traceables
    ( -- * Traces
      HitPoint(..)
    , hitPoint
    , trace
    -- * Traceable bodies
    , Body
    -- ** Primitives
    , plane
    , sphere
    , cylinder
    , cone
    -- ** Compositions
    , intersect
    , unite
    , complement
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst, snd, reverse)

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
--
-- Using strict tuple performs better: 100 traces for 350K
-- particles perform roughly 7s against 8s with common datatypes.
type Trace = [HitSegment]

-- | A segment on time line when particle is inside the body.
type HitSegment = (Pair HitPoint HitPoint)

-- | Time when particle hits the surface, along with normal at hit
-- point. If hit is in infinity, then normal is Nothing.
--
-- Note that this datatype is strict only on first argument: we do not
-- compare normals when classifying traces.
data HitPoint = HitPoint !Double (Maybe Vec3)
                deriving (Eq, Ord, Show)


-- | IEEE positive infinity.
infinityP :: Double
infinityP = (/) 1 0


-- | Negative infinity.
infinityN :: Double
infinityN = -infinityP


hitN :: HitPoint
hitN = (HitPoint infinityN Nothing)


hitP :: HitPoint
hitP = (HitPoint infinityP Nothing)


-- | CSG body is a recursive composition of primitive objects or other
-- bodies.
data Body = Plane !Vec3 !Double
          -- ^ Halfspace with normalized outward normal and distance
          -- from origin.
          | Sphere !Vec3 !Double
          | Cylinder !Vec3 !Point !Double
          -- ^ Cylinder with normalized axis vector, point on axis and
          -- radius.
          | Cone !Vec3 !Point !Double
          -- | Cone given by axis direction, vertex and angle between
          -- axis and outer edge (in radians).
          | Union !Body !Body
          -- ^ Union of bodies.
          | Intersection !Body !Body
          -- ^ Intersection of bodies.
          | Complement !Body
            deriving Show


-- | Half-space defined by plane with outward normal and distance from
-- origin.
plane :: Vec3 -> Double -> Body
plane n d = Plane (normalize n) d


-- | Sphere defined by center point and radius.
sphere :: Vec3 -> Double -> Body
sphere o r = Sphere o r


-- | Infinite cylinder defined by vector collinear to axis, point on
-- axis and radius.
cylinder :: Vec3 -> Point -> Double -> Body
cylinder a o r = Cylinder (normalize a) o r

cone :: Vec3 -> Point -> Double -> Body
cone a o h = Cone (normalize a) o h

intersect :: Body -> Body -> Body
intersect !b1 !b2 = Intersection b1 b2


unite :: Body -> Body -> Body
unite !b1 !b2 = Union b1 b2


complement :: Body -> Body
complement !b = Complement b


-- | Trace a particle on a body.
trace :: Body -> Particle -> Trace
{-# INLINE trace #-}

trace !(Plane n d) !(pos, v) =
    let
        !f = -(n .* v)
    in
      -- Check if ray is parallel to plane
      if f == 0
      then []
      else
          let
              !t = (pos .* n - d) / f
          in
            if f > 0
            then [(HitPoint t (Just n)) :!: (HitPoint infinityP Nothing)]
            else [(HitPoint infinityN Nothing) :!: (HitPoint t (Just n))]

trace !(Sphere c r) !(pos, v) =
      let
          !d = pos <-> c
          !roots = solveq (v .* v) (v .* d * 2) (d .* d - r * r)
          normal !u = normalize (u <-> c)
      in
        case roots of
          Nothing -> []
          Just (t1 :!: t2) ->
              [HitPoint t1 (Just $ normal $ moveBy pos v t1) :!:
               HitPoint t2 (Just $ normal $ moveBy pos v t2)]

trace !(Cylinder n c r) !(pos, v) =
    let
        r2 = r * r
        d = (pos <-> c) >< n
        e = v >< n
        roots = solveq (e .* e) (d .* e * 2) (d .* d - r2)
        normal u = normalize $ h <-> (n .^ (h .* n))
            where h = u <-> c
    in
      case roots of
        Nothing -> []
        Just (t1 :!: t2) ->
            [HitPoint t1 (Just $ normal $ moveBy pos v t1) :!:
                      HitPoint t2 (Just $ normal $ moveBy pos v t2)]

trace !(Cone n c a) !(pos, v) =
    let
      nn = normalize n
      a' = cos $! a
      gamma = diag (-a' * a')
      delta = pos <-> c
      m = addM (nn `vxv` nn) gamma
      c2 = dotM v     v     m
      c1 = dotM v     delta m
      c0 = dotM delta delta m
      roots = solveq c2 (2 * c1) c0
      normal u = normalize $ nx .^ (1 / ta)  <-> ny .^ ta
          where h = u <-> c
                -- Component of h parallel to cone axis
                ny' = nn .^ (nn .* h)
                ny = normalize ny'
                -- Perpendicular component
                nx = normalize $ h <-> ny'
                ta = tan $! a
    in
      case roots of
        Nothing -> []
        Just (t1 :!: t2) ->
            let
                pos1 = moveBy pos v t1
                pos2 = moveBy pos v t2
                odelta = c .* n
            in
              case ((pos1 .* n + odelta) > 0, (pos1 .* n + odelta) > 0) of
                (True, True) -> [HitPoint t1 (Just $ normal pos1) :!:
                                 HitPoint t2 (Just $ normal pos2)]
                (True, False) -> [HitPoint infinityN Nothing :!:
                                  HitPoint t1 (Just $ normal pos1)]
                (False, True) -> [HitPoint t2 (Just $ normal pos2) :!:
                                  HitPoint infinityP Nothing]
                (False, False) -> []

trace !(Intersection b1 b2) !p =
    intersectTraces tr1 tr2
        where
          tr1 = trace b1 p
          tr2 = trace b2 p

trace !(Union b1 b2) !p =
    uniteTraces tr1 tr2
        where
          tr1 = trace b1 p
          tr2 = trace b2 p

trace !(Complement b) !p =
    complementTraces tr
        where
          tr = trace b p

uniteTraces :: Trace -> Trace -> Trace
uniteTraces u [] = u
uniteTraces u (v:t2) =
      uniteTraces (unite1 u v) t2
      where
        merge :: HitSegment -> HitSegment -> HitSegment
        merge !(a1 :!: b1) !(a2 :!: b2) = (min a1 a2) :!: (max b1 b2)
        {-# INLINE merge #-}
        unite1 :: Trace -> HitSegment -> Trace
        unite1 [] hs = [hs]
        unite1 t@(hs1@(a1 :!: b1):tr') hs2@(a2 :!: b2)
            | b1 < a2 = hs1:(unite1 tr' hs2)
            | a1 > b2 = hs2:t
            | otherwise = unite1 tr' (merge hs1 hs2)
        {-# INLINE unite1 #-}
{-# INLINE uniteTraces #-}


intersectTraces :: Trace -> Trace -> Trace
intersectTraces tr1 tr2 =
    let
        -- Overlap two overlapping segments
        overlap :: HitSegment -> HitSegment -> HitSegment
        overlap !(a1 :!: b1) !(a2 :!: b2) = (max a1 a2) :!: (min b1 b2)
        {-# INLINE overlap #-}
    in
      case tr2 of
        [] -> []
        (hs2@(a2 :!: b2):tr2') ->
            case tr1 of
              [] -> []
              (hs1@(a1 :!: b1):tr1') ->
                  case (b1 < a2) of
                    True -> (intersectTraces tr1' tr2)
                    False ->
                        case (b2 < a1) of
                          True -> intersectTraces tr1 tr2'
                          False -> (overlap hs1 hs2):(intersectTraces tr1' tr2)
{-# INLINE intersectTraces #-}


-- | Complement to trace (normals flipped) in @R^3@.
complementTraces :: Trace -> Trace
complementTraces ((sp@(HitPoint ts _) :!: ep):xs) =
    start ++ (complementTraces' ep xs)
    where
      flipNormals :: HitSegment -> HitSegment
      flipNormals !((HitPoint t1 n1) :!: (HitPoint t2 n2)) =
          (HitPoint t1 (reverse <$> n1)) :!: (HitPoint t2 (reverse <$> n2))
      {-# INLINE flipNormals #-}
      -- Start from infinity if first hitpoint is finite
      start = if (isInfinite ts)
              then []
              else [flipNormals $ hitN :!: sp]
      complementTraces' :: HitPoint -> Trace -> Trace
      complementTraces' c ((a :!: b):tr) =
          -- Bridge between last point of previous segment and first
          -- point of the next one.
          (flipNormals (c :!: a)):(complementTraces' b tr)
      complementTraces' a@(HitPoint t _) [] =
          -- End in infinity if last hitpoint is finite
          if (isInfinite t)
          then []
          else [flipNormals (a :!: hitP)]
complementTraces [] = [hitN :!: hitP]
{-# INLINE complementTraces #-}


-- | If particle has hit the body during last time step, calculate the
-- corresponding 'HitPoint'. Note that time at which hit occured will
-- be negative.
hitPoint :: Time -> Body -> Particle -> Maybe HitPoint
hitPoint !dt !b !p =
    let
        lastHit = [(HitPoint (-dt) Nothing) :!: (HitPoint 0 Nothing)]
    in
      case (intersectTraces lastHit $ trace b p) of
        [] -> Nothing
        (hs:_) -> Just $ fst hs
{-# INLINE hitPoint #-}
