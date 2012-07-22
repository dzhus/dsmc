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
    , initializeSamples
    , updateSamples
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Control.Monad.ST

import Data.Strict.Maybe

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import DSMC.Cells
import DSMC.Particles
import DSMC.Util.Vector

-- | Macroscopic parameters calculated in every cell: mean absolute
-- velocity, mean square of thermal velocity and particle count.
type MacroParameters = (Vec3, Double, Int)


-- | Vector which stores macroscropic parameters in each cell for
-- every time step calculated during macroscopic sampling collection
-- step.
--
-- We store samples for time steps linearly for consecutive time
-- steps, so if there're @N@ cells and @M@ time steps, the size of
-- array is @N*M@, where first N elements store samples from the first
-- time step, next N elements store samples from the second etc.
--
-- If the vector is reshaped into two-dimensional array, then first
-- dimension is time step index, where 0 is the time step when
-- averaging starts; second dimension is the cell index as returned by
-- function produced with corresponding 'makeRegularIndexer'.
type MacroSamples = VU.Vector MacroParameters
--type MacroSamples = R.Array R.U R.DIM2 MacroParameters


-- | Sampling cell is attached to certain point in space and has a
-- number of specimen.
type MacroCell = (Point, CellContents)


-- | Flow velocity.
type Velocity = Vec3


-- | A function which calculates instantaneous macroscopic average of
-- a parameter using molecules in a cell.
type CellSampler a = CellContents -> Maybe a


-- | Parameters in empty cell.
emptySample :: MacroParameters
emptySample = ((0, 0, 0), 0, 0)


-- | Create empty 'MacroSamples' array.
initializeSamples :: Int
                  -- ^ For how many time steps to collect samples.
                  -> MacroSamples
initializeSamples n = VU.replicate n emptySample


-- | Gather samples from ensemble.
updateSamples :: Monad m =>
                 Int
              -- ^ Index of current time step, with 0 being the step
              -- when first sample is gathered.
              -> (Int, Classifier)
              -- ^ Cell count and classifier.
              -> Ensemble
              -> MacroSamples
              -- ^ Current sample information to be updated.
              -> m MacroSamples
updateSamples n sorting ens samples =
    let
        !sorted = sortParticles sorting ens
        -- Sampling results from current step
    in do
      return samples
    

-- | Sample macroscopic values in a cell.
sampleMacroscopic :: CellContents -> MacroParameters
sampleMacroscopic !ens =
    case VU.null ens of
      True -> emptySample
      False ->
          let
              -- Particle count
              n = VU.length ens
              s = 1 / fromIntegral n
              -- Mean absolute velocity
              m1 = (VU.foldl' (\v0 (_, v) -> v0 <+> v) (0, 0, 0) ens) .^ s
              -- Mean square thermal velocity
              c2 = (VU.foldl' (+) 0 $ 
                      VU.map (\(_, v) ->
                                  let
                                    thrm = (v <-> m1)
                                  in
                                    (thrm .* thrm))
                      ens) * s
          in
            (m1, c2, n)


printVels :: V.Vector (Maybe (Point, Velocity)) -> IO ()
printVels samples =
    V.forM_ samples
         (\mb -> do case mb of
                      Nothing -> return ()
                      Just ((x, y, z), (u, v, w)) ->
                          putStrLn $ unwords (map show [x, y, z, u, v, w]))
