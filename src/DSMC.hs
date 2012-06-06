{-# LANGUAGE BangPatterns #-}

{-|

Simulation procedures.

-}
module DSMC
    ( advance
    , step
    , simulate
    )

where

import Control.Monad.Primitive (PrimMonad)

import qualified Data.Array.Repa as R

import Data.Strict.Maybe as S

import System.Random.MWC

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


-- Monadic because of Repa's parallel computeP.
step :: Monad m =>
        DomainSeed
     -> Domain
     -> Body
     -> Flow
     -> Time
     -> Double
     -> Ensemble
     -> m (Ensemble, DomainSeed)
step seeds domain body flow dt ex ens =
    do
      let !(e, seeds') = openBoundaryInjection seeds domain ex flow ens
      e' <- advance dt body e >>= clipToDomain domain
      return (e', seeds')


simulate :: PrimMonad m =>
            Domain
         -> Body
         -> Flow
         -> Time
         -> Time
         -> Double
         -> m Ensemble
simulate domain body flow dt tmax ex =
    let
        -- Helper which runs simulation until current time exceeds
        -- limit
        sim1 seeds tcur ens =
            if tcur > tmax then return ens
               else do
                 (ens', seeds') <- step seeds domain body flow dt ex ens
                 sim1 seeds' (tcur + dt) ens'
    in do
      s <-  create >>= save
      s1 <- create >>= save
      s2 <- create >>= save
      s3 <- create >>= save
      s4 <- create >>= save
      s5 <- create >>= save
      s6 <- create >>= save
      sim1 (s1, s2, s3, s4, s5, s6) 0 $ initialParticles s domain flow
