{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Particle operations.

-}

module DSMC.Particles
    ( Particle
    , Ensemble
    , ParEnsemble
    , move
    , printEnsemble
    )

where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

import DSMC.Types
import DSMC.Util.Vector


-- | Gas particle with position and velocity.
type Particle = (Point, Vec3)


-- | Linearly move particle for t time and update its position.
move :: Time -> Particle -> Particle
move dt (pos, v) = (pos <+> (v .^ dt), v)
{-# INLINE move #-}


-- | Array of particles.
type Ensemble = VU.Vector Particle


-- | Repa array of particles.
type ParEnsemble = R.Array R.U R.DIM1 Particle


-- | Print particles, one per row, using the format:
--
-- > x y z v u w
printEnsemble :: Ensemble -> IO ()
printEnsemble particles = do
  VU.forM_ particles
        (\((x, y, z), (u, v, w)) -> putStrLn $ unwords (map show [x, y, z, u, v, w]))
