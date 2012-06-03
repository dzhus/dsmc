-- | Molecular velocity distributions

module DSMC.Distributions

where

import Control.Monad.Primitive (PrimMonad, PrimState)

import System.Random.MWC
import System.Random.MWC.Distributions

import DSMC.Util.Vector

-- | Sample molecular velocity vector for bulk flow with given
-- macroscopic velocity and gas parameters.
genVelocity :: PrimMonad m => Gen (PrimState m) -> m Vec3
genVelocity g = do
  u <- standard g
  v <- standard g
  w <- standard g
  return $! Vec3 u v w
