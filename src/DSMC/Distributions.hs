{-# LANGUAGE BangPatterns #-}

-- | Molecular velocity distributions

module DSMC.Distributions

where

import Control.Monad.Primitive (PrimMonad, PrimState)

import System.Random.MWC
import System.Random.MWC.Distributions (normal)

import DSMC.Util.Vector
import DSMC.Constants


-- | Sample molecular velocity vector for flow with given macroscopic
-- velocity and gas parameters.
genVelocity :: PrimMonad m =>
               Gen (PrimState m) 
            -> Double 
            -- ^ Thermodynamic temperature.
            -> Double
            -- ^ Molecular mass.
            -> Vec3
            -- ^ Flow velocity.
            -> m Vec3
genVelocity !g !t !m !(x, y, z) =
    let
        !s = sqrt $ boltzmann * t / m
    in do
      u <- normal x s g
      v <- normal y s g
      w <- normal z s g
      return $! (u, v, w)
