{-|

Volume operations

-}

module DSMC.Volume
    ( clipVolume
    )

where

import DSMC.Types
import DSMC.Util.Vector


-- | Test if particle is in domain.
inVolume :: Volume -> Particle -> Bool
inVolume (Box xmin xmax ymin ymax zmin zmax) (Particle (Vec3 x y z) _) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin


-- | Filter out particles which are not in domain.
clipVolume :: Volume -> [Particle] -> [Particle]
clipVolume vol particles = filter (inVolume vol) particles
