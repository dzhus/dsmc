{-|
  
  Simulation procedures.

-}
module DSMC
    ()

where

import DSMC.Particles
import DSMC.Types
import DSMC.Util.Vector


-- | Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vec3 -> Time -> Particle
reflectSpecular (pos, v) n t =
    move (-1 * t) (pos, v <-> (n .^ (v .* n) .^ 2))
