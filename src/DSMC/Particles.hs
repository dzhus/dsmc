module DSMC.Particles

where

import Vector

data Particle = Particle
                {
                  position :: Point,
                  speed :: Vector
                }
                deriving Show

-- Move particle for t time and update its position
move t (Particle p speed) = (Particle (p <+> (speed *> t)) speed)
