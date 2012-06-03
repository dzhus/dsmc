{-# LANGUAGE BangPatterns #-}

{-|

Domain operations

-}

module DSMC.Domain
    ( spawnParticles,
      Domain(..)
    )

where

import Control.Monad.Primitive (PrimMonad, PrimState)

import Data.Array.Repa
import qualified Data.Vector.Unboxed as U

import System.Random.MWC
import System.Random.MWC.Distributions (normal)

import DSMC.Constants
import DSMC.Particles
import DSMC.Types
import DSMC.Util.Vector


-- | Domain in which particles are spawned or system evolution is
-- simulated.
data Domain = Box !Double !Double !Double !Double !Double !Double
            -- ^ Rectangular volume, given by min/max value on every
            -- dimension.
              deriving Show


-- | Test if particle is in domain.
inDomain :: Domain -> Particle -> Bool
inDomain !(Box xmin xmax ymin ymax zmin zmax) !((x, y, z), _) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin


-- | Measure volume of domain.
volume :: Domain -> Double
volume !(Box xmin xmax ymin ymax zmin zmax) =
    (xmax - xmin) * (ymax - ymin) * (zmax - zmin)


-- | Sample new particles in domain.
spawnParticles :: PrimMonad m =>
                  Gen (PrimState m)
               -> Domain
               -> Double
               -- ^ Concentration
               -> Double
               -- ^ Thermodynamic temperature.
               -> Double
               -- ^ Molecular mass.
               -> Vec3
               -- ^ Flow velocity.
               -> m (U.Vector Particle)
spawnParticles !g !d@(Box xmin xmax ymin ymax zmin zmax) !n !t !m !(u0, v0, w0) =
    let
        !s = sqrt $ boltzmann * t / m
        !count = round $ n * (volume d)
    in do
      U.replicateM count $ do
         u <- normal u0 s g
         v <- normal v0 s g
         w <- normal w0 s g
         x <- uniformR (xmin, xmax) g
         y <- uniformR (ymin, ymax) g
         z <- uniformR (zmin, zmax) g
         return ((x, y, z), (u, v, w))
