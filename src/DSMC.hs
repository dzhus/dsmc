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
import Control.Monad.Primitive (PrimMonad)

import Control.Parallel.Stochastic

import Data.Functor

import qualified Data.Array.Repa as R

import qualified Data.Strict.Maybe as S

import qualified Data.Vector.Unboxed as VU

import System.Random.MWC

import DSMC.Cells
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Particles
import DSMC.Surface
import DSMC.Traceables hiding (trace)
import DSMC.Util

import Control.Monad.ST

import Debug.Trace


-- | Sequential action to move particles and consider particle-body
-- collisions.
reflect :: GenST s
        -> Body
        -> Time
        -> Reflector s
        -> VU.Vector Particle
        -> ST s (VU.Vector Particle)
reflect g body dt reflector ens = do
  VU.forM ens $ \pcl -> do
    -- Particle after collisionless motion
    let movedPcl = move dt pcl
    case (hitPoint dt body movedPcl) of
      -- Enjoy your convex-only case.
      S.Just (HitPoint th (S.Just n)) ->
          let
              -- Position and velocity at hit point
              (pos', v) = move th pcl
          in do
            -- Sample velocity for reflected particle
            vR <- reflector g n v
            -- Move particle away from surface with new velocity
            return $ move (-th) (pos', vR)
      _ -> return $ movedPcl


-- | Collisionless motion step.
motion :: ParallelSeeds
       -> Body
       -> Time
       -> Surface
       -> Ensemble
       -> (Ensemble, ParallelSeeds)
motion gs b dt surf ens =
    let
        vs :: [VU.Vector Particle]
        -- | Since 'reflect' is sequential, we split ensemble into N
        -- slices and process them in parallel.
        !vs = splitIn (R.toUnboxed ens) (length gs)
        reflector = makeReflector surf
        v' :: [(VU.Vector Particle, Seed)]
        !v' = parMapST (\g e -> reflect g b dt reflector e) $ zip vs gs
    in
      (fromUnboxed1 $ VU.concat $ fst <$> v', snd <$> v')


-- | Perform DSMC simulation.
simulate :: PrimMonad m =>
            Domain
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
         -- ^ Split Lagrangian step into that many independent
         -- parallel processes.
         -> m Ensemble
simulate domain body flow dt emptyStart ex sepsilon ssteps (mx, my, mz) gsplit =
    let
        -- Simulate evolution of the particle system for one time
        -- step, updating seeds used for sampling stochastic
        -- processes.
        step :: Monad m =>
                (Ensemble, ParallelSeeds, DomainSeeds)
             -> m (Ensemble, ParallelSeeds, DomainSeeds)
        step (ens, gseeds, dseeds) =
            do
              let
                  -- Inject new particles
                  (e, dseeds') = openBoundaryInjection dseeds domain ex flow ens
                  -- Lagrangian step
                  (e', gseeds') = motion gseeds body dt (CLL 500 0.1 0.3) e
              
              -- Filter out particles which left the domain
              e'' <- clipToDomain domain e'
              
              return $! (trace (show $ R.extent e'') e'', gseeds', dseeds')

        macroSubdiv :: UniformGrid
        macroSubdiv = UniformGrid domain mx my mz

        macroOpts = MacroSamplingOptions
                    (makeUniformClassifier macroSubdiv)
                    (makeUniformIndexer macroSubdiv)
                    ssteps

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
             -> MacroSamplingMonad Ensemble
        sim1 !oldState@(ens, _, _) !steady = do
            !newState@(ens', _, _) <- step oldState
            let !newSteady = steady || stabilized ens' ens

            !enough <- case steady of
                         False -> return False
                         True -> updateSamples ens'

            case enough of
              False -> sim1 newState newSteady
              True -> return ens'
    in do
      gs <- replicateM gsplit $ create >>= save
      s1 <- create >>= save
      s2 <- create >>= save
      s3 <- create >>= save
      s4 <- create >>= save
      s5 <- create >>= save
      s6 <- create >>= save

      startEnsemble <- if emptyStart 
                       then return emptyEnsemble
                       else do
                         -- Forget the initial sampling seed
                         r <- create >>= save >>= 
                              initializeParticles domain flow body
                         return $ fst r

      return $ fst $ startMacroSampling 
                 (sim1 (startEnsemble, gs, (s1, s2, s3, s4, s5, s6)) False)
                 macroOpts
