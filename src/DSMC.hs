{-# LANGUAGE BangPatterns #-}

{-|

Simulation procedures.

-}
module DSMC
    ( advance
    )

where

import qualified Data.Array.Repa as R

import Data.Strict.Maybe as S

import DSMC.Cells
import DSMC.Domain
import DSMC.Particles
import DSMC.Traceables
import DSMC.Types
import DSMC.Util.Vector


-- | Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Time -> Vec3 -> Particle
reflectSpecular !(pos, v) !t !n =
    move (-1 * t) (pos, v <-> (n .^ (v .* n) .^ 2))


-- | Advance particles in space without intermolecular collisions, but
-- reflect particles which have collided with body.
advance :: Monad m =>
           Time
        -> Body
        -> Ensemble
        -> m Ensemble
advance !dt !b ens =
    let
        moved = R.map (move dt) ens
        maybeReflect = (\(!p) ->
                            case (hitPoint dt b p) of
                              S.Just (HitPoint t (S.Just n)) ->
                                  reflectSpecular p t n
                              _ -> p)
        {-# INLINE maybeReflect #-}
        -- Reflect those particles which are not
        reflected = R.map maybeReflect moved
    in
      R.computeP $ reflected


-- | Classify points into cells of regular grid with given spatial
-- steps.
makeRegularClassifier :: Domain
                      -> Double
                      -- ^ X step.
                      -> Double
                      -- ^ Y step.
                      -> Double
                      -- ^ Z step.
                      -> Classifier
makeRegularClassifier d@(Domain xmin _ ymin _ zmin _)
                      hx hy hz =
    let
        (w, l, _) = getDimensions d
        xsteps = ceiling $ w / hx
        ysteps = ceiling $ l / hy
        classify ((x, y, z), _) =
            let
                nx = floor $ (x - xmin) / hx
                ny = floor $ (y - ymin) / hy
                nz = floor $ (z - zmin) / hz
            in
              nx + ny * xsteps + nz * xsteps * ysteps
    in
      classify
