{-|

Simulation domain definitions.

-}

module DSMC.Domain

where

import DSMC.Particles


-- | Domain in which particle system evolution is simulated.
data Domain = Box Double Double Double Double Double Double
              deriving Show


-- | Test if particle is in domain.
inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) _) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin


-- | Filter out particles which are not in domain.
clipDomain :: Domain -> [Particle] -> [Particle]
clipDomain domain particles = filter (inDomain domain) particles
