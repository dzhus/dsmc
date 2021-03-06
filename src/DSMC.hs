{-# LANGUAGE BangPatterns #-}

{-|

DSMC is an algorithm used for simulating rarefied gas flows.

You define the simulation domain, the body inside this domain, gas
flow parameters and several other options. DSMC iteratively models the
behaviour of gas molecules according to time and space decoupling
scheme for the Boltzmann equation. The result of simulation is a field
of macroscopic parameters across the simulation domain.

-}

module DSMC
    ( motion
    , simulate
    )

where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor

import System.Log.Logger

import Control.Parallel.Stochastic

import DSMC.Cells
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Motion
import DSMC.Particles
import DSMC.Surface hiding (mass)
import DSMC.Traceables hiding (trace)
import DSMC.Util


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
         -> Surface
         -- ^ Model for surface of body.
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
         surface
         (mx, my, mz) volumePoints gsplit =
    let
        -- Simulate evolution of the particle system for one time
        -- step, updating seeds used for sampling stochastic
        -- processes.
      evolve :: (Ensemble, ParallelSeeds, DomainSeeds)
             -> (Ensemble, ParallelSeeds, DomainSeeds)
      evolve (ens, gseeds, dseeds) =
          let
            -- Inject new particles
            (e, dseeds') = openBoundaryInjection dseeds domain ex flow ens

            -- Lagrangian step
            (e', gseeds') = motion gseeds body dt surface e

            -- Filter out particles which left the domain
            e'' = clipToDomain domain e'
          in
            (e'', gseeds', dseeds')

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
      sim1 !oldState@(ens, _, _) steady n =
        let
          !newState@(ens', _, _) = evolve oldState
          !newSteady = steady || stabilized ens' ens
        in do
          !enough <- case steady of
            False -> return False
            True -> updateSamples ens'
          liftIO $ debugM rootLoggerName $
                   (if steady
                   then "Steady state"
                   else "Not steady yet") ++
                   "; particles count: " ++
                   (show $ ensembleSize ens')
          case enough of
            False -> sim1 newState newSteady (n + 1)
            True -> do
              (Just field) <- getField (mass flow) (statWeight flow)
              return (n, ens', field)
    in do
      -- Global seeds
      gs <- replicateM gsplit $ randomSeed

      -- Interface domain seeds
      (s1:s2:s3:s4:s5:s6:_) <- replicateM 6 randomSeed

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
      fst <$> runMacroSampling
              (sim1
               (startEnsemble, gs, (s1, s2, s3, s4, s5, s6))
               False 0)
              vs macroSubdiv body volumePoints ssteps
