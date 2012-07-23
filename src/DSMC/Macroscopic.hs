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
    , MacroSamplingMonad
    , MacroSamplingOptions(..)
    )

where

import Prelude hiding (Just, Nothing, Maybe)

import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

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
--
-- Note the lack of root on thermal velocity!
type MacroParameters = (Int, Vec3, Double)


-- | Vector which stores macroscropic parameters in each cell for
-- every time step calculated during macroscopic sampling collection
-- step.
--
-- We store samples for the whole domain linearly for consecutive time
-- steps, so if there're @N@ cells and @M@ time steps for averaging,
-- the size of array is @N*M@, where first N elements store samples
-- from the first time step, next N elements store samples from the
-- second etc.
--
-- Parameters sampled in @i@-th cell on @j@-th step are stored under
-- index @j*N+i@.
--
-- If the vector is reshaped into two-dimensional array, then first
-- dimension is the time step index, where 0 is the time step when
-- averaging starts; second dimension is the cell index as returned by
-- function produced with corresponding 'makeRegularIndexer'.
type MacroSamples = VU.Vector MacroParameters


-- | Monad which keeps track of sampling process data and stores
-- options of macroscopic sampling.
--
-- State contains averaging steps left and current samples. If
-- Nothing, then averaging has not started yet.
--
-- We use this to make sure that only safe values for cell count and
-- time steps are used in 'updateSamples' and 'averageSamples' (that
-- may otherwise cause unbounded access errors).
type MacroSamplingMonad =
    StateT (Maybe (Int, MacroSamples)) (Reader MacroSamplingOptions)


-- | Options for macroscopic sampling: uniform grid and averaging
-- steps count.
data MacroSamplingOptions = 
    MacroSamplingOptions { _sorting :: (Int, Classifier)
                         , _indexer :: Int -> Point
                         , _averagingSteps :: Int
                         }


-- | Parameters in empty cell.
emptySample :: MacroParameters
emptySample = (0, (0, 0, 0), 0)


-- | Create empty 'MacroSamples' array.
initializeSamples :: Int
                  -- ^ Cell count.
                  -> Int
                  -- ^ Averaging steps.
                  -> MacroSamples
initializeSamples cellCount ts = VU.replicate (ts * cellCount) emptySample


-- | Gather samples from ensemble. Return True if sampling is
-- finished, False otherwise.
updateSamples :: Ensemble
              -> MacroSamplingMonad Bool
updateSamples ens = do
  samples <- get

  sorting@(cellCount, _) <- lift $ asks _sorting

  maxSteps <- lift $ asks _averagingSteps

  let (n, oldSamples) =
          case samples of
            Nothing -> (maxSteps, initializeSamples cellCount maxSteps)
            -- Just (0, _) -> error
            Just o -> o
      -- Sort particles into macroscopic cells for sampling
      sorted = sortParticles sorting ens
      -- Starting index for current step in grand vector of samples
      stepStart = (maxSteps - n) * cellCount
      -- Update samples vector
      newSamples = runST $ do
        -- Sampling results from current step
        stepSamples <- R.computeP $
                        cellMap (\_ c -> sampleMacroscopic c) sorted

        samples' <- VU.unsafeThaw oldSamples
        iforM_ (R.toUnboxed $ stepSamples)
                   (\(i, macroParams) -> do
                      VUM.write samples' (stepStart + i) macroParams
                      return ())
        VU.unsafeFreeze samples'
  -- Update state of sampling process
  put $ Just (n - 1, newSamples)

  return $ (n - 1) == 0

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
