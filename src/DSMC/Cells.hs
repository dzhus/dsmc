{-

Particle tracking for spatial grid for DSMC.

This module is used to sort particles into ordered vector of cells for
collision step or macroscopic parameter sampling.

-}

module DSMC.Cells
    ( CellContents
    , Classifier
    , sortParticles
    )

where

import Control.Monad.ST
import qualified Data.Array.Repa as R

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import DSMC.Particles


-- | Cell contents with particles.
type CellContents = VU.Vector Particle


-- | Every particle belongs to some cell and can be given the unique
-- index inside this cell.
type CellMapping = (Int, Int)


-- | Assuming there's a linear ordering on all cells, Classifier must
-- yield index of cell for given particle.
type Classifier = Particle -> Int


-- | Pre-calculate particle classification.
--
-- Yield classification for ensemble and vector of cell lengths.
classifyAll :: Int
            -> Classifier
            -> VU.Vector Particle
            -> ST s (VU.Vector CellMapping, VU.Vector Int)
classifyAll cellCount classify ens = do
  lengths <- VUM.replicate cellCount 0

  classes <- VU.forM ens (\p -> do
      let cellNumber = classify p
      -- Increment cell particle count
      count <- VUM.read lengths cellNumber
      VUM.write lengths cellNumber (count + 1)
      return $ (cellNumber, count))

  lengths' <- VU.unsafeFreeze lengths
  return $ (classes, lengths')


-- | Sort particle ensemble into `N` cells using the classifier
-- function. Cells are stored as irregular two-dimensional array,
-- using mutable 'Data.Vector.Unboxed.Vector' operations internally.
-- 
-- Classifier extent must match `N`, yielding numbers between
-- `0`..`N-1`.
sortParticles :: Int 
              -> Classifier 
              -> Ensemble 
              -> ST s (V.Vector CellContents)
sortParticles cellCount classify ens = do
  let ens' = R.toUnboxed ens
  -- Mutable vector of sequences
  (classes, lengths) <- classifyAll cellCount classify ens'

  -- Assign proper sizes to second dimension.
  --
  -- Somewhy mutable vectors have no generateM.
  cells' <- V.generateM cellCount $ \j -> do
                 VUM.unsafeNew ((VU.!) lengths j)
  cells <- V.unsafeThaw cells'

  -- Classify particles stored in unboxed vector into cellCount.
  VU.zipWithM_ (\p (c, i) -> do
                  cell <- VM.read cells c
                  VUM.write cell i p
                  VM.write cells c cell) ens' classes

  -- Freeze top-level mutable
  V.generateM cellCount (\j -> do
                           cell <- VM.unsafeRead cells j
                           VU.unsafeFreeze cell)
