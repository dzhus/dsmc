{-# LANGUAGE BangPatterns #-}

{-|

Domain operations

-}

module DSMC.Domain
    ( Domain(..)
    , makeDomain
    , openBoundaryInjection
    , spawnParticles
    )

where

import Control.Monad.Primitive (PrimMonad, PrimState)

import qualified Data.Vector.Unboxed as VU

import System.Random.MWC
import System.Random.MWC.Distributions (normal)

import DSMC.Constants
import DSMC.Particles
import DSMC.Types
import DSMC.Util.Vector


-- | Domain in which particles are spawned or system evolution is
-- simulated.
data Domain = Domain !Double !Double !Double !Double !Double !Double
            -- ^ Rectangular volume, given by min/max value on every
            -- dimension.
              deriving Show


-- | Test if particle is in domain.
inDomain :: Domain -> Particle -> Bool
inDomain !(Domain xmin xmax ymin ymax zmin zmax) !((x, y, z), _) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin


-- | Create a rectangular domain with center in the given point and
-- dimensions.
makeDomain :: Point
        -- ^ Center point.
        -> Double
        -- ^ X dimension.
        -> Double
        -- ^ Y dimension.
        -> Double
        -- ^ Z dimension.
        -> Domain
makeDomain !(x, y, z) !w !l !h =
    let
        xmin = x - w / 2
        ymin = y - l / 2
        zmin = z - h / 2
        xmax = x + w / 2
        ymax = y + l / 2
        zmax = z + h / 2
    in
      Domain xmin xmax ymin ymax zmin zmax


-- | Calculate width, length and height of a domain, which are
-- dimensions measured by x, y and z axes, respectively.
getDimensions :: Domain -> (Double, Double, Double)
getDimensions !(Domain xmin xmax ymin ymax zmin zmax) =
    (xmax - xmin, ymax - ymin, zmax - zmin)


-- | Calculate geometric center of a domain.
getCenter :: Domain -> Point
getCenter !(Domain xmin xmax ymin ymax zmin zmax) =
    (xmin + (xmax - xmin) / 2, ymin + (ymax - ymin) / 2, zmin + (zmax - zmin) / 2)


-- | Measure volume of domain.
volume :: Domain -> Double
volume !(Domain xmin xmax ymin ymax zmin zmax) =
    (xmax - xmin) * (ymax - ymin) * (zmax - zmin)


-- | Sample new particles inside a domain.
spawnParticles :: PrimMonad m =>
                  Gen (PrimState m)
               -> Domain
               -> Double
               -- ^ Concentration.
               -> Double
               -- ^ Thermodynamic temperature.
               -> Double
               -- ^ Mass of molecule.
               -> Vec3
               -- ^ Flow velocity.
               -> m Ensemble
spawnParticles !g !d@(Domain xmin xmax ymin ymax zmin zmax) !n !t !m !(u0, v0, w0) =
    let
        !s = sqrt $ boltzmann * t / m
        !count = round $ n * (volume d)
    in do
      VU.replicateM count $ do
         u <- normal u0 s g
         v <- normal v0 s g
         w <- normal w0 s g
         x <- uniformR (xmin, xmax) g
         y <- uniformR (ymin, ymax) g
         z <- uniformR (zmin, zmax) g
         return $! ((x, y, z), (u, v, w))


-- | Sample new particles in 6 interface domains along each side of
-- rectangular simulation domain.
--
--
-- This function implements open boundary condition for
-- three-dimensional simulation domain.
--
-- Interface domains are built on faces of simulation domain using
-- extrusion along the outward normal of the face.
--
-- In 2D projection:
-- >          +-----------------+
-- >          |    Interface1   |
-- >       +--+-----------------+--+
-- >       |I3|    Simulation   |I4|
-- >       |  |      domain     |  |
-- >       +--+-----------------+--+
-- >          |        I2       |
-- >          +-----------------+
openBoundaryInjection :: PrimMonad m =>
                         Gen (PrimState m)
                      -> Domain
                      -- ^ Simulation domain.
                      -> Double
                      -- ^ Interface domain extrusion length.
                      -> Double
                      -- ^ Concentration.
                      -> Double
                      -- ^ Thermodynamic temperature.
                      -> Double
                      -- ^ Mass of a molecule.
                      -> Vec3
                      -- ^ Flow velocity.
                      -> m Ensemble
openBoundaryInjection !g !d@(Domain xmin xmax ymin ymax zmin zmax)
                      !ex !n !t !m !flow =
    let
        (w, l, h) = getDimensions d
        (cx, cy, cz) = getCenter d
        d1 = makeDomain (cx - (w + ex) / 2, cy, cz) ex l h
        d2 = makeDomain (cx + (w + ex) / 2, cy, cz) ex l h
        d3 = makeDomain (cx, cy + (l + ex) / 2, cz) w ex h
        d4 = makeDomain (cx, cy - (l + ex) / 2, cz) w ex h
        d5 = makeDomain (cx, cy, cz - (h + ex) / 2) w l ex
        d6 = makeDomain (cx, cy, cz + (h + ex) / 2) w l ex
        (++) = (VU.++)
    in do
      v1 <- spawnParticles g d1 n t m flow
      v2 <- spawnParticles g d2 n t m flow
      v3 <- spawnParticles g d3 n t m flow
      v4 <- spawnParticles g d4 n t m flow
      v5 <- spawnParticles g d5 n t m flow
      v6 <- spawnParticles g d6 n t m flow
      return $ v1 ++ v2 ++ v3 ++ v4 ++ v5 ++ v6
