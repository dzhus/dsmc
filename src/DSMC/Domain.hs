{-|

Simulation domain operations.

-}

module DSMC.Domain
    ( clipDomain
    )

where

import DSMC.Types
import DSMC.Util.Vector


-- | Test if particle is in domain.
inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (Vec3 x y z) _) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin


-- | Filter out particles which are not in domain.
clipDomain :: Domain -> [Particle] -> [Particle]
clipDomain domain particles = filter (inDomain domain) particles
