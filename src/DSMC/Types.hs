{-|

DSMC definitions.

-}

module DSMC.Types
    ( Time
    , Particle(..)
    , Domain(..)
    )

where

import DSMC.Util.Vector


-- | Time in seconds.
type Time = Double


-- | Gas particle with position and velocity.
data Particle = Particle
                {
                  position :: Point,
                  velocity :: Vector
                }
                deriving Show


-- | Domain in which particle system evolution is simulated.
data Domain = Box Double Double Double Double Double Double
              deriving Show
