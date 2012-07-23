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

import qualified Data.Array.Repa as R

import qualified Data.Strict.Maybe as S

import qualified Data.Vector.Unboxed as VU

import System.Random.MWC

import DSMC.Cells
import DSMC.Domain
import DSMC.Macroscopic
import DSMC.Particles
import DSMC.Surface
import DSMC.Traceables
import DSMC.Types
import DSMC.Util

import Control.Monad.ST

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


type GlobalSeeds = [Seed]


-- | Collisionless motion step.
motion :: GlobalSeeds
       -> Body
       -> Time
       -> Surface
       -> Ensemble
       -> (Ensemble, GlobalSeeds)
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
      (fromUnboxed1 $ VU.concat $ map fst v', map snd v')


-- | Perform DSMC simulation.
simulate :: PrimMonad m =>
            Domain
         -> Body
         -> Flow
         -> Time
         -- ^ Time step.
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
simulate domain body flow dt ex sepsilon ssteps (mx, my, mz) gsplit =
    let
        -- Simulate evolution of the particle system for one time
        -- step, updating seeds used for sampling stochastic
        -- processes.
        step :: Monad m =>
                GlobalSeeds
             -> DomainSeeds
             -> Ensemble
             -> m (Ensemble, GlobalSeeds, DomainSeeds)
        step gseeds dseeds ens =
            do
              let -- Inject new particles
                  !(e, dseeds') = openBoundaryInjection dseeds domain ex flow ens
                  -- Lagrangian step
                  !(e', gseeds') = motion gseeds body dt (CLL 500 0.1 0.3) e
              -- Filter out particles which left the domain
              !e'' <- clipToDomain domain e'
              return $! (e'', gseeds', dseeds')

        -- Classifier for spatial grid used to sample macroscopic parameters.
        macroClassifier :: (Int, Classifier)
        macroClassifier@(cellCount, _) = makeRegularClassifier (domain, mx, my, mz)

        -- Helper which runs simulation until enough samples in steady
        -- state are collected
        sim1 :: Monad m => 
                GlobalSeeds 
             -> DomainSeeds 
             -> Ensemble 
             -> MacroSamples 
             -> Maybe Int
             -- ^ Simulation iterations till exit, or Nothing if
             -- steady regime has not yet been reached.
             -> m Ensemble
        sim1 gseeds dseeds ens samples q =
            case q of
              Just 0 -> return ens
              _ -> do
                (ens', gseeds', dseeds') <- step gseeds dseeds ens

                -- Check if number of particles in the system has stabilized
                !(newQ, samples') <-
                    case q of
                      Nothing -> return $
                                 if ((abs $ (fromIntegral $ ensembleSize ens') / (fromIntegral $ ensembleSize ens)) - 1) < sepsilon
                                 then (Just ssteps, samples)
                                 else (Nothing, samples)
                      Just n -> return $ (Just $ n - 1, samples)

                sim1 gseeds' dseeds' ens' samples' newQ
    in do
      gs <- replicateM gsplit $ create >>= save
      s1 <- create >>= save
      s2 <- create >>= save
      s3 <- create >>= save
      s4 <- create >>= save
      s5 <- create >>= save
      s6 <- create >>= save
      sim1 gs (s1, s2, s3, s4, s5, s6) emptyEnsemble (initializeSamples cellCount ssteps) Nothing
