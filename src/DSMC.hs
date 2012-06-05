{-# LANGUAGE BangPatterns #-}

{-|

Simulation procedures.

-}
module DSMC
    ( advance
    , hit)

where

import qualified Data.Array.Repa as R

import Data.Strict.Maybe as S

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


-- | Advance particles in space without collisions.
--
-- We use Repa parallel 'R.map', thus it's a monadic action.
advance :: Monad m =>
           Double
        -- ^ Time step.
        -> Ensemble
        -> m Ensemble
advance dt ens = R.computeP $ R.map (move dt) ens


-- | Reflect particles which have collided with body.
hit :: Monad m =>
       Time
    -> Body
    -> Ensemble
    -> m Ensemble
hit dt b ens =
    let
        hits = R.map (hitPoint dt b) ens
        maybeReflect = (\(!p) (!h) ->
                            case h of
                              S.Just (HitPoint t (S.Just n)) ->
                                  reflectSpecular p t n
                              _ -> p)
        {-# INLINE maybeReflect #-}
        -- Reflect those particles which are not
        reflected = R.zipWith maybeReflect ens hits
    in
      R.computeP $ reflected
