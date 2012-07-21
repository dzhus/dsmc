{-# LANGUAGE BangPatterns #-}

{-|

Simulation procedures.

-}
module DSMC
    ( advance
    , step
    , simulate
    )

where

import Control.Monad
import Control.Monad.Primitive (PrimMonad)

import qualified Data.Array.Repa as R

import Data.Strict.Maybe as S

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
advance :: GlobalSeeds
        -> Body
        -> Time
        -> Surface
        -> Ensemble
        -> (Ensemble, GlobalSeeds)
advance gs b dt surf ens =
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


-- | Monadic because of Repa's parallel computeP.
step :: Monad m =>
        GlobalSeeds
     -> DomainSeeds
     -> Domain
     -> Body
     -> Flow
     -> Time
     -> Double
     -> Ensemble
     -> m (Ensemble, GlobalSeeds, DomainSeeds)
step gseeds dseeds domain body flow dt ex ens =
    do
      let !(e, dseeds') = openBoundaryInjection dseeds domain ex flow ens
          !(e', gseeds') = advance gseeds body dt (CLL 500 0.1 0.3) e
      !e'' <- clipToDomain domain e'
      return $! (e'', gseeds', dseeds')


-- | Perform DSMC simulation.
simulate :: PrimMonad m =>
            Domain
         -> Body
         -> Flow
         -> Time
         -> Time
         -> Double
         -> Int
         -- ^ Split Lagrangian step into that many independent
         -- parallel processes.
         -> m Ensemble
simulate domain body flow dt tmax ex gsplit =
    let
        -- Helper which runs simulation until current time exceeds
        -- limit
        sim1 gseeds dseeds tcur ens =
            if tcur > tmax then return ens
               else do
                 (ens', gseeds', dseeds') <- step gseeds dseeds domain body flow dt ex ens
                 sim1 gseeds' dseeds' (tcur + dt) ens'
    in do
      gs <- replicateM gsplit $ create >>= save
      s1 <- create >>= save
      s2 <- create >>= save
      s3 <- create >>= save
      s4 <- create >>= save
      s5 <- create >>= save
      s6 <- create >>= save
      sim1 gs (s1, s2, s3, s4, s5, s6) 0 emptyEnsemble
