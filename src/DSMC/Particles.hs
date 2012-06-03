{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Particle operations.

-}

module DSMC.Particles
    ( Particle(..)
    , move
    )

where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import DSMC.Util.Vector

import DSMC.Types
import DSMC.Util.Vector


-- | Gas particle with position and velocity.
type Particle = (Point, Vec3)


-- | Linearly move particle for t time and update its position.
move :: Time -> Particle -> Particle
move dt p@(pos, v) = (pos <+> (v .^ dt), v)
