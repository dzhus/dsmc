{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Particle operations.

-}

module DSMC.Particles
    ( Particle
    , Ensemble
    , move
    )

where

import qualified Data.Vector.Unboxed as VU

import DSMC.Types
import DSMC.Util.Vector


-- | Gas particle with position and velocity.
type Particle = (Point, Vec3)


-- | Linearly move particle for t time and update its position.
move :: Time -> Particle -> Particle
move dt p@(pos, v) = (pos <+> (v .^ dt), v)


-- | Array of particles.
type Ensemble = VU.Vector Particle
