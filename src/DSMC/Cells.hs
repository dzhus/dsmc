{-|

Particle tracking for spatial grid for DSMC.

This module is used to sort particles into ordered vector of cells for
collision step or macroscopic parameter sampling. We do not provide
any special cell datatype since it varies which cell data is required
on every step.

-}

module DSMC.Cells
    ( CellContents
    , Classifier
    , sortParticles
    -- * Regular subdivision
    , RegularSubdivision
    , makeRegularClassifier
    , makeRegularIndexer
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
import DSMC.Util.Vector


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


-- | Domain divided in regular grid with given steps by X, Y and Z
-- axes.
type RegularSubdivision = (Domain, Double, Double, Double)


-- | Classify particles into cells of regular grid with given spatial
-- steps.
makeRegularClassifier :: RegularSubdivision
                      -> Classifier
makeRegularClassifier (d@(Domain xmin _ ymin _ zmin _), hx, hy, hz) =
    classify
    where
        (w, l, _) = getDimensions d
        xsteps = ceiling $ w / hx
        ysteps = ceiling $ l / hy
        classify ((x, y, z), _) =
            let
                nx = floor $ (x - xmin) / hx
                ny = floor $ (y - ymin) / hy
                nz = floor $ (z - zmin) / hz
            in
              nx + ny * xsteps + nz * xsteps * ysteps


-- | Indexing function which maps cell numbers to central points of
-- regular cells.
makeRegularIndexer :: RegularSubdivision
                   -> Int -> Point
makeRegularIndexer (d@(Domain xmin _ ymin _ zmin _), hx, hy, hz) =
    indefy
    where
        (w, l, _) = getDimensions d
        xsteps = ceiling $ w / hx
        ysteps = ceiling $ l / hy
        zf = xsteps * ysteps

        indefy i =
            let
                (nz, i') = i `divMod` zf
                z = zmin + fromIntegral nz * hz + hz / 2

                (ny, nx) = i' `divMod` ysteps
                y = ymin + fromIntegral ny * hy + hy / 2
                x = xmin + fromIntegral nx * hx + hx / 2
            in
              (x, y, z)
