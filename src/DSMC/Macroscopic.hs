{-# LANGUAGE BangPatterns #-}

{-|

Macroscopic parameters calculation.

We use regular spatial grid and time averaging for sampling. Sampling
should start after particle system has reached steady state. Samples
are then collected in each cell for a certain number of time steps.

-}

module DSMC.Macroscopic
    ( MacroCell
    , Velocity
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Control.Monad.ST

import Data.Strict.Maybe

import qualified Data.Array.Repa as R
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import DSMC.Cells
import DSMC.Particles
import DSMC.Util.Vector


-- | Macroscopic parameters calculated in every cell: 1st raw moment
-- for particle velocity, 2nd central moment for particle velocity and
-- particle count.
type MacroParameters = (Vec3, Vec3, Int)


-- | Two-dimensional array which stores macroscropic parameters in
-- each cell for every time step calculated during macroscopic
-- sampling collection step. First dimension is time step index, where
-- 0 is the time step when averaging starts; second dimension is the
-- cell index as returned by function produced with corresponding
-- 'makeRegularIndexer'.
type MacroSamples = R.Array R.U R.DIM2 MacroParameters


-- | Sampling cell is attached to certain point in space and has a
-- number of specimen.
type MacroCell = (Point, CellContents)


-- | Flow velocity.
type Velocity = Vec3


-- | A function which calculates instantaneous macroscopic average of
-- a parameter using molecules in a cell.
type CellSampler a = CellContents -> Maybe a


-- | Sample velocity inside a cell.
sampleVelocity :: CellSampler Velocity
sampleVelocity !ens =
    case VU.null ens of
      True -> Nothing
      False -> Just $ (VU.foldl' (\v0 (_, v) -> v0 <+> v) (0, 0, 0) ens) .^ 
                      (1 / fromIntegral (VU.length ens))


printVels :: V.Vector (Maybe (Point, Velocity)) -> IO ()
printVels samples =
    V.forM_ samples
         (\mb -> do case mb of
                      Nothing -> return ()
                      Just ((x, y, z), (u, v, w)) ->
                          putStrLn $ unwords (map show [x, y, z, u, v, w]))
