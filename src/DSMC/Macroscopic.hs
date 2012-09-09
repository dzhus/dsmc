{-# LANGUAGE BangPatterns #-}

{-|

Macroscopic parameters calculation.

We use regular spatial grid and time averaging for sampling. Sampling
should start after particle system has reached steady state. Samples
are then collected in each cell for a certain number of time steps.

Sampling is performed in 'MacroSamplingMonad' to ensure consistency of
averaging process.

-}

module DSMC.Macroscopic
    ( MacroSamples
    , MacroField
    , MacroParameters
    -- * Macroscopic sampling monad
    , MacroSamplingMonad
    , SamplingState(..)
    , runMacroSampling
    , updateSamples
    , getField
    )

where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import qualified Data.Strict.Maybe as S

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

import Control.Parallel.Stochastic

import DSMC.Cells
import DSMC.Particles
import DSMC.Traceables
import DSMC.Util
import DSMC.Util.Vector


-- | Macroscopic parameters calculated in every cell: particle count,
-- mean absolute velocity, mean square of thermal velocity.
--
-- Particle count is non-integer because of averaging.
--
-- These are then post-processed into number density, flow velocity,
-- pressure and translational temperature.
--
-- Note the lack of root on thermal velocity!
type MacroParameters = (Double, Vec3, Double)


-- | Vector which stores averaged macroscropic parameters in each
-- cell.
--
-- If samples are collected for M iterations, then this vector is
-- built as a sum of vectors @V1, .. VM@, where @Vi@ is vector of
-- parameters sampled on @i@-th time step divided by @M@.
type MacroSamples = R.Array R.U R.DIM1 MacroParameters


-- | Array of central points of grid cells with averaged macroscopic
-- parameters attached to every point.
type MacroField = R.Array R.U R.DIM1 (Point, MacroParameters)


-- | Monad which keeps track of sampling process data and stores
-- options of macroscopic sampling.
--
-- GridMonad is used to ensure that only safe values for cell count
-- and classifier are used in 'updateSamples' and 'averageSamples'
-- (that may otherwise cause unbounded access errors). Note that
-- steady condition is not handled by this monad (instead, caller code
-- should decide when to start averaging).
--
-- Inner Reader Monad stores averaging steps setting.
type MacroSamplingMonad =
    StateT SamplingState (ReaderT Int GridMonad)


-- | State of sampling process.
data SamplingState = None
                   -- ^ Sampling has not started yet.
                   | Incomplete Int MacroSamples
                   -- ^ Sampling is in progress, not enough samples
                   -- yet. Integer field indicates how many steps are
                   -- left.
                   | Complete MacroSamples
                   -- ^ Averaging is complete, use 'getField' to
                   -- unload the samples.


-- | Fetch macroscopic field if averaging is complete.
getField :: MacroSamplingMonad (Maybe MacroField)
getField = do
  (cellCount, _) <- lift $ lift $ asks classifier
  ixer <- lift $ lift $ asks indexer
  res <- get
  case res of
    Complete samples -> do
             let centralPoints = R.fromFunction (R.ix1 $ cellCount)
                                 (\(R.Z R.:. cellNumber) -> ixer cellNumber)
             f <- R.computeP $ R.zipWith (,) centralPoints samples
             return $ Just f
    _ -> return $ Nothing


-- | Parameters in empty cell.
emptySample :: MacroParameters
emptySample = (0, (0, 0, 0), 0)


-- | Run 'MacroSamplingMonad' action with given sampling options and
-- return final 'Complete' state with macroscopic samples.
runMacroSampling :: MacroSamplingMonad r
                 -> ParallelSeeds
                 -> Grid
                 -- ^ Grid used to sample macroscopic parameters.
                 -> Body
                 -> Int
                 -- ^ Use that many points to approximate every cell volume.
                 -> Int
                 -- ^ Averaging steps count.
                 -> DSMCRootMonad (r, SamplingState)
runMacroSampling f seeds grid body testPoints ssteps = 
    runGrid (runReaderT (runStateT f None) ssteps) seeds grid body testPoints


-- | Create empty 'MacroSamples' array.
initializeSamples :: Int
                  -- ^ Cell count.
                  -> MacroSamples
initializeSamples cellCount = fromUnboxed1 $
                              VU.replicate cellCount emptySample


-- | Gather samples from ensemble. Return True if sampling is
-- finished, False otherwise.
updateSamples :: Ensemble
              -> MacroSamplingMonad Bool
updateSamples ens =
    let
        addCellParameters :: MacroParameters
                          -> MacroParameters
                          -> MacroParameters
        addCellParameters !(n1, v1, c1) !(n2, v2, c2) =
            (n1 + n2, v1 <+> v2, c1 + c2)
    in do
      sorting@(cellCount, _) <- lift $ lift $ asks classifier

      maxSteps <- lift $ ask

      sampling <- get

      -- n is steps left for averaging
      let (n, oldSamples) =
              case sampling of
                None -> (maxSteps, initializeSamples cellCount)
                Incomplete s o -> (s, o)
                Complete _ -> error "updateSamples called, but pool's closed."
          weight = 1 / fromIntegral maxSteps
          -- Sort particles into macroscopic cells for sampling
          sorted = classifyParticles sorting ens
          -- Sampling results from current step
          stepSamples = cellMap (\_ c -> sampleMacroscopic c weight) sorted
       -- Add samples from current step to all sum of samples collected so
       -- far
      !newSamples <- R.computeP $
                     R.zipWith addCellParameters oldSamples stepSamples

      let fin = (n == 0)

      -- Update state of sampling process
      put $ case fin of 
              True -> Complete newSamples
              False -> Incomplete (n - 1) newSamples

      return fin


-- | Sample macroscopic values in a cell.
sampleMacroscopic :: S.Maybe CellContents
                  -> Double
                  -- ^ Multiply all sampled parameters by this number,
                  -- which is the statistical weight of one sample.
                  -- Typically this is inverse to the amount of steps
                  -- used for averaging.
                  -> MacroParameters
sampleMacroscopic !c !weight =
    case c of
      S.Nothing -> emptySample
      S.Just ens ->
          let
              -- Particle count
              n = fromIntegral $ VU.length ens
              -- Particle averaging factor
              s = 1 / n
              -- Velocities factor
              sv = s * weight
              -- Mean absolute velocity
              m1 = (VU.foldl' (\v0 (_, v) -> v0 <+> v) (0, 0, 0) ens) .^ sv
              -- Mean square thermal velocity
              c2 = (VU.foldl' (+) 0 $
                      VU.map (\(_, v) ->
                                  let
                                    thrm = (v <-> m1)
                                  in
                                    (thrm .* thrm))
                      ens) * sv
          in
            (n * weight, m1, c2)
