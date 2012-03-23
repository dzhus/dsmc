{-|

Particle definitions.

-}

module DSMC.Particles

where

import DSMC.Util
import DSMC.Util.Vector


-- | Gas particle with position and velocity.
data Particle = Particle
                {
                  position :: Point,
                  velocity :: Vector
                }
                deriving Show


-- | Linearly move particle for t time and update its position.
move :: Time -> Particle -> Particle
move dt p@(Particle pos velocity) =
    p{position = pos <+> (velocity *> dt)}
