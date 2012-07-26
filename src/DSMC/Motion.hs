{-# LANGUAGE BangPatterns #-}

{-|

Collisionless motion.

-}

module DSMC.Motion
    ( motion
    )

where

import Control.Parallel.Stochastic

import qualified Data.Strict.Maybe as S

import qualified Data.Array.Repa as R

import qualified Data.Vector.Unboxed as VU

import System.Random.MWC

import DSMC.Particles
import DSMC.Surface
import DSMC.Traceables hiding (trace)
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


-- | Collisionless motion step.
motion :: ParallelSeeds
       -> Body
       -> Time
       -> Surface
       -> Ensemble
       -> (Ensemble, ParallelSeeds)
motion gs b dt surf ens =
    let
        -- | Since 'reflect' is sequential, we split ensemble into N
        -- slices and process them in parallel.
        reflector = makeReflector surf
        !(v', newSeeds) = 
            splitParMapST (\g e -> reflect g b dt reflector e)
                          (R.toUnboxed ens) gs
    in
      (fromUnboxed1 v', newSeeds)

