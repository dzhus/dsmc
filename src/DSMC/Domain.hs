{-# LANGUAGE BangPatterns #-}

{-|

Domain operations: defining domains; free flow boundary conditions &
clipping for DSMC steps.

PRNG required to sample molecular velocities implies monadic interface
for most of operations. We use functions specifically typed for 'ST'.

-}

module DSMC.Domain
    ( Domain(..)
    , getDimensions
    , getCenter
    , makeDomain
    -- * Flow boundary
    , initializeParticles
    , openBoundaryInjection
    , DomainSeeds
    , clipToDomain
    -- * Free volume calculation
    , freeVolume
    , freeVolumes
    )

where

import Control.Monad.ST

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import System.Random.MWC
import System.Random.MWC.Distributions (normal)

import Control.Parallel.Stochastic

import DSMC.Particles
import DSMC.Traceables
import DSMC.Util
import DSMC.Util.Constants
import DSMC.Util.Vector


-- | Domain in which particles are spawned or system evolution is
-- simulated.
data Domain = Domain !Double !Double !Double !Double !Double !Double
              -- ^ Rectangular volume, given by min/max value on every
              -- dimension.
              deriving Show


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
{-# INLINE makeDomain #-}


-- | PRNG seeds used by particle generators.
type DomainSeeds = (Seed, Seed, Seed, Seed, Seed, Seed)


-- | Calculate width, length and height of a domain, which are
-- dimensions measured by x, y and z axes, respectively.
getDimensions :: Domain -> (Double, Double, Double)
getDimensions (Domain xmin xmax ymin ymax zmin zmax) =
    (xmax - xmin, ymax - ymin, zmax - zmin)
{-# INLINE getDimensions #-}


-- | Calculate geometric center of a domain.
getCenter :: Domain -> Point
getCenter (Domain xmin xmax ymin ymax zmin zmax) =
    (xmin + (xmax - xmin) / 2, ymin + (ymax - ymin) / 2, zmin + (zmax - zmin) / 2)
{-# INLINE getCenter #-}


-- | Volume of domain.
volume :: Domain -> Double
volume !(Domain xmin xmax ymin ymax zmin zmax) =
    (xmax - xmin) * (ymax - ymin) * (zmax - zmin)
{-# INLINE volume #-}


-- | Sample new particles inside a domain.
--
-- PRNG state implies this to be a monadic action.
spawnParticles :: Domain
               -> Flow
               -> GenST s
               -> ST s (VU.Vector Particle)
spawnParticles d@(Domain xmin xmax ymin ymax zmin zmax) flow g =
    let
        s = sqrt $ boltzmann * (temperature flow) / (mass flow)
        (u0, v0, w0) = velocity flow
        count = round $ (modelConcentration flow) * (volume d)
    in do
      VU.replicateM count $ do
         u <- normal u0 s g
         v <- normal v0 s g
         w <- normal w0 s g
         x <- uniformR (xmin, xmax) g
         y <- uniformR (ymin, ymax) g
         z <- uniformR (zmin, zmax) g
         return $ ((x, y, z), (u, v, w))


-- | Pure version of 'spawnParticles'.
pureSpawnParticles :: Domain
                   -> Flow
                   -> Seed
                   -> (VU.Vector Particle, Seed)
pureSpawnParticles d flow s = purifyRandomST (spawnParticles d flow) s


-- | Fill the domain with particles for given flow parameters.
-- Particles inside the body are removed.
initializeParticles :: Domain
                    -> Flow
                    -> Body
                    -> Seed
                    -> (Ensemble, Seed)
initializeParticles d flow body s =
    let
        (res, s') = pureSpawnParticles d flow s
        ens = fromUnboxed1 res
    in 
      (filterEnsemble (not . inside body) ens, s')


-- | Sample new particles in 6 interface domains along each side of
-- rectangular simulation domain and add them to existing ensemble.
--
-- This function implements open boundary condition for
-- three-dimensional simulation domain.
--
-- Interface domains are built on faces of simulation domain using
-- extrusion along the outward normal of the face.
--
-- In 2D projection:
--
-- >          +-----------------+
-- >          |    Interface1   |
-- >       +--+-----------------+--+
-- >       |I3|    Simulation   |I4|
-- >       |  |      domain     |  |
-- >       +--+-----------------+--+
-- >          |        I2       |
-- >          +-----------------+
--
-- Particles in every interface domain are spawned in parallel using
-- Strategies.
openBoundaryInjection :: DomainSeeds
                      -> Domain
                      -- ^ Simulation domain.
                      -> Double
                      -- ^ Interface domain extrusion length.
                      -> Flow
                      -> Ensemble
                      -> (Ensemble, DomainSeeds)
openBoundaryInjection (s1, s2, s3, s4, s5, s6) domain ex flow ens =
    let
        (w, l, h) = getDimensions domain
        (cx, cy, cz) = getCenter domain
        d1 = makeDomain (cx - (w + ex) / 2, cy, cz) ex l h
        d2 = makeDomain (cx + (w + ex) / 2, cy, cz) ex l h
        d3 = makeDomain (cx, cy + (l + ex) / 2, cz) w ex h
        d4 = makeDomain (cx, cy - (l + ex) / 2, cz) w ex h
        d5 = makeDomain (cx, cy, cz - (h + ex) / 2) w l ex
        d6 = makeDomain (cx, cy, cz + (h + ex) / 2) w l ex
        v = [R.toUnboxed ens]
        (new, (s1':s2':s3':s4':s5':s6':_)) =
            unzip $
            parMapST (\g d -> spawnParticles d flow g) $
            zip [d1, d2, d3, d4, d5, d6] [s1, s2, s3, s4, s5, s6]
    in
      (fromUnboxed1 $ VU.concat (new ++ v), (s1', s2', s3', s4', s5', s6'))


-- | Filter out particles which are outside of the domain.
clipToDomain :: Domain -> Ensemble -> Ensemble
clipToDomain (Domain xmin xmax ymin ymax zmin zmax) ens =
    let
        -- | Check if particle is in the domain.
        pred' :: Particle -> Bool
        pred' !((x, y, z), _) =
            xmax >= x && x >= xmin &&
            ymax >= y && y >= ymin &&
            zmax >= z && z >= zmin
        {-# INLINE pred' #-}
    in 
      filterEnsemble pred' ens


-- | Volume of a domain unoccupied by a given body, in m^3.
--
-- We use Monte Carlo method to calculate the approximate body volume
-- and then subtract it from the overall domain volume.
freeVolume :: Domain 
           -> Body 
           -> Int 
           -- ^ Use that many points to approximate the body volume.
           -> GenST s
           -> ST s (Double)
freeVolume d@(Domain xmin xmax ymin ymax zmin zmax) body testPoints g = do
  points <- VU.replicateM testPoints $ do
              x <- uniformR (xmin, xmax) g
              y <- uniformR (ymin, ymax) g
              z <- uniformR (zmin, zmax) g
              return $ inside body ((x, y, z), (0, 0, 0))
  let occupiedPoints = VU.length $ VU.filter id points
  return $ (volume d) * 
             (fromIntegral (testPoints - occupiedPoints)) /
             (fromIntegral testPoints)


-- | Sequential 'freeVolume' for a vector of domains.
freeVolumes :: Body
            -> Int
            -> GenST s
            -> V.Vector Domain
            -> ST s (VU.Vector Double)
freeVolumes body testPoints g doms =
    VU.generateM (V.length doms)
          (\i -> freeVolume (doms V.! i) body testPoints g)
