{-# LANGUAGE BangPatterns #-}

{-|

Simulation procedures.

-}

module DSMC
    ( motion
    , simulate
    )

where

import Control.Monad

import qualified Data.Array.Repa as R

import Control.Parallel.Stochastic

import DSMC.Cells
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Motion
import DSMC.Particles
import DSMC.Surface
import DSMC.Traceables hiding (trace)
import DSMC.Util
import Debug.Trace


-- | Perform DSMC simulation, return total iterations count, final
-- particle distribution and field of averaged macroscopic parameters.
--
-- This is an IO action since system entropy source is polled for
-- seeds.
simulate :: Domain
         -> Body
         -> Flow
         -> Time
         -- ^ Time step.
         -> Bool
         -- ^ If true, start with empty domain. Add initial particle
         -- distribution to the domain otherwise.
         -> Double
         -- ^ Source reservoir extrusion.
         -> Double
         -- ^ Steadiness epsilon.
         -> Int
         -- ^ Step count limit in steady regime.
         -> (Double, Double, Double)
         -- ^ Spatial steps in X, Y, Z of grid used for macroscopic
         -- parameter sampling.
         -> Int
         -- ^ Use that many test points to calculate volume of every
         -- cell wrt body. Depends on Knudsen number calculated from
         -- cell size.
         -> Int
         -- ^ Split Lagrangian step into that many independent
         -- parallel processes.
         -> IO (Int, Ensemble, MacroField)
simulate domain body flow
         dt emptyStart ex sepsilon ssteps
         (mx, my, mz) volumePoints gsplit =
    let
        -- Simulate evolution of the particle system for one time
        -- step, updating seeds used for sampling stochastic
        -- processes.
      evolve :: Monad m =>
                (Ensemble, ParallelSeeds, DomainSeeds)
             -> m (Ensemble, ParallelSeeds, DomainSeeds)
      evolve (ens, gseeds, dseeds) =
        do
          let
            -- Inject new particles
            (e, dseeds') = openBoundaryInjection dseeds domain ex flow ens
            
            -- Lagrangian step
            (e', gseeds') = motion gseeds body dt (CLL 500 0.1 0.3) e

            -- Filter out particles which left the domain
            e'' = clipToDomain domain e'

          return $! (e'', gseeds', dseeds')

      macroSubdiv :: Grid
      macroSubdiv = UniformGrid domain mx my mz

      -- Check if two consecutive particle ensemble states
      -- correspond to steady regime.
      stabilized :: Ensemble -> Ensemble -> Bool
      stabilized ens prevEns =
        (abs $
         ((fromIntegral $ ensembleSize ens) /
          (fromIntegral $ ensembleSize prevEns) - 1)) < sepsilon
      
      -- Helper which actually runs simulation and collects
        -- macroscopic data until enough samples in steady state are
      -- collected.
      sim1 :: (Ensemble, ParallelSeeds, DomainSeeds)
           -> Bool
           -- ^ True if steady regime has been reached.
           -> Int
           -- ^ Iteration counter.
           -> MacroSamplingMonad (Int, Ensemble, MacroField)
      sim1 !oldState@(ens, _, _) steady n = do
        !newState@(ens', _, _) <- evolve oldState
        let !newSteady = steady || stabilized ens' ens

        !enough <- case steady of
                     False -> return False
                     True -> updateSamples ens'

        case enough of
          False -> sim1 newState newSteady (n + 1)
          True -> do
            (Just field) <- getField
            return (n, ens', field)
    in do
      -- Global seeds
      gs <- replicateM gsplit $ randomSeed

      -- Interface domain seeds
      s1 <- randomSeed
      s2 <- randomSeed
      s3 <- randomSeed
      s4 <- randomSeed
      s5 <- randomSeed
      s6 <- randomSeed

      -- Seeds for cell volume calculation
      vs <- replicateM gsplit $ randomSeed

      -- Possibly sample initial particle distribution
      startEnsemble <- if emptyStart
                       then return emptyEnsemble
                       else do
                         -- Forget the initial sampling seed
                         is <- randomSeed
                         return $ fst $ initializeParticles domain flow body is

      -- Start the process
      return $ fst $ runMacroSampling
                 (sim1
                  (startEnsemble, gs, (s1, s2, s3, s4, s5, s6))
                  False 0)
                 vs macroSubdiv body volumePoints ssteps
