{-|

Simulation domain operations.

-}

module DSMC.Domain
    ( clipDomain
    )

where

import DSMC.Types

-- | Test if particle is in domain.
inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) _) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin


-- | Filter out particles which are not in domain.
clipDomain :: Domain -> [Particle] -> [Particle]
clipDomain domain particles = filter (inDomain domain) particles
