module Domain

where

import Particles

data Domain = Box Double Double Double Double Double Double
              deriving Show

inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) v) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin         

clipDomain domain particles = filter (inDomain domain) particles
