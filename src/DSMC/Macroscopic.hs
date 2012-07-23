{-# LANGUAGE BangPatterns #-}

{-|

Macroscopic parameters calculation.

We use regular spatial grid and time averaging for sampling. Sampling
should start after particle system has reached steady state. Samples
are then collected in each cell for a certain number of time steps.

-}

module DSMC.Macroscopic
    ( MacroSamples
    , initializeSamples
    , updateSamples
    )

where

import Prelude hiding (Just, Nothing, Maybe)

import Control.Monad.ST

import Data.Strict.Maybe

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import DSMC.Cells
import DSMC.Particles
import DSMC.Util
import DSMC.Util.Vector

-- | Macroscopic parameters calculated in every cell: particle count,
-- mean absolute velocity, mean square of thermal velocity.
type MacroParameters = (Int, Vec3, Double)


-- | Vector which stores macroscropic parameters in each cell for
-- every time step calculated during macroscopic sampling collection
-- step.
--
-- We store samples for the whole domain linearly for consecutive time
-- steps, so if there're @N@ cells and @M@ time steps, the size of
-- array is @N*M@, where first N elements store samples from the first
-- time step, next N elements store samples from the second etc.
--
-- Parameters sampled in @i@-th cell on @j@-th step are stored under
-- index @j*N+i@.
--
-- If the vector is reshaped into two-dimensional array, then first
-- dimension is the time step index, where 0 is the time step when
-- averaging starts; second dimension is the cell index as returned by
-- function produced with corresponding 'makeRegularIndexer'.
type MacroSamples = VU.Vector MacroParameters


-- | Parameters in empty cell.
emptySample :: MacroParameters
emptySample = (0, (0, 0, 0), 0)


-- | Create empty 'MacroSamples' array.
initializeSamples :: Int
                  -- ^ Cell count.
                  -> Int
                  -- ^ For how many time steps to collect samples.
                  -> MacroSamples
initializeSamples cellCount ts = VU.replicate (ts * cellCount) emptySample


-- | Gather samples from ensemble.
--
-- Not that bounds are not checked.
updateSamples :: Monad m =>
                 Int
              -- ^ Index of current time step, with 0 being the step
              -- when first sample is gathered.
              -> (Int, Classifier)
              -- ^ Cell count and classifier.
              -> Ensemble
              -> MacroSamples
              -- ^ Current sample information to be updated, if
              -- there's any.
              -> m MacroSamples
updateSamples n sorting@(cellCount, _) ens oldSamples =
    let
        !sorted = sortParticles sorting ens
        -- Starting index for current step in grand vector of samples
        !stepStart = n * cellCount
    in return $ runST $ do
        -- Sampling results from current step
      !stepSamples <- R.computeP $
                      cellMap (\_ c -> sampleMacroscopic c) sorted

      samples' <- VU.unsafeThaw oldSamples
      iforM_ (R.toUnboxed $ stepSamples)
                 (\(i, macroParams) -> do
                    VUM.write samples' (stepStart + i) macroParams
                    return ())
      samples <- VU.unsafeFreeze samples'

      return samples
    

-- | Sample macroscopic values in a cell.
sampleMacroscopic :: Maybe CellContents -> MacroParameters
sampleMacroscopic !c =
    case c of
      Nothing -> emptySample
      Just ens ->
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
            (n, m1, c2)
