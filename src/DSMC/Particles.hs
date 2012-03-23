{-|

Particle operations.

-}

module DSMC.Particles

where

import DSMC.Types
import DSMC.Util.Vector


-- | Linearly move particle for t time and update its position.
move :: Time -> Particle -> Particle
move dt p@(Particle pos velocity) =
    p{position = pos + (velocity `scale` dt)}
