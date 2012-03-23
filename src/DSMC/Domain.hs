{-|

Simulation domain definitions.

|-}

module DSMC.Domain

where

import DSMC.Particles

data Domain = Box Double Double Double Double Double Double
              deriving Show

-- | Test if particle is in domain.
inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) v) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin

-- | Filter out particles which are not in domain.
clipDomain domain particles = filter (inDomain domain) particles
