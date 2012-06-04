{-|

  Simulation procedures.

-}
module DSMC
    (advance)

where

import qualified Data.Array.Repa as R

import DSMC.Particles
import DSMC.Types
import DSMC.Util.Vector


-- | Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vec3 -> Time -> Particle
reflectSpecular (pos, v) n t =
    move (-1 * t) (pos, v <-> (n .^ (v .* n) .^ 2))


-- | Advance particles in space without collisions.
advance :: Monad m =>
           Double
        -- ^ Time step.
        -> Ensemble
        -> m Ensemble
advance dt ens = R.computeP $ R.map (move dt) ens
