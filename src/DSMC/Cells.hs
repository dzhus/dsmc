{-|

Particle tracking for spatial grid for DSMC.

This module is used to sort particles into ordered vector of cells for
collision step or macroscopic parameter sampling.

-}

module DSMC.Cells
    ( Cell(..)
    , CellContents
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

import DSMC.Domain
import DSMC.Particles


-- | Cell has certain location in space and contains particles.
data Cell = Cell !Domain !CellContents


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
      count <- VUM.unsafeRead lengths cellNumber
      VUM.unsafeWrite lengths cellNumber (count + 1)
      -- Yield cell number and index of particle in the cell
      return $ (cellNumber, count))

  lengths' <- VU.unsafeFreeze lengths
  return $ (classes, lengths')


-- | Sort particle ensemble into @N@ cells using the classifier
-- function. Cells are stored as irregular two-dimensional array,
-- using mutable 'Data.Vector.Unboxed.Vector' operations internally.
--
-- Classifier extent must match @N@, yielding numbers between @0@ and
-- @N-1@.
sortParticles :: Int
              -- ^ Number of cells.
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
                  cell <- VM.unsafeRead cells c
                  VUM.unsafeWrite cell i p
                  VM.unsafeWrite cells c cell) ens' classes

  -- Freeze top-level mutable
  V.generateM cellCount (\j -> do
                           cell <- VM.unsafeRead cells j
                           VU.unsafeFreeze cell)
