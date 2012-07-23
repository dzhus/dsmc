{-# LANGUAGE BangPatterns #-}

{-|

Particle tracking for regular spatial grid for DSMC.

This module is used to sort particles into ordered vector of cells for
collision step or macroscopic parameter sampling. We do not provide
any special cell datatype since it varies which cell data is required
on every step, so only particles in every cell are stored.

-}

module DSMC.Cells
    ( -- * Generic functions for cells
      Cells
    , CellContents
    , getCell
    , cellMap
    -- * Particle tracking
    , Classifier
    , sortParticles
    -- * Regular subdivision
    , RegularSubdivision
    , makeRegularClassifier
    , makeRegularIndexer
    )

where

import Prelude hiding (Just, Nothing, Maybe)

import Control.Monad.ST

import Data.Strict.Maybe

import qualified Data.Array.Repa as R

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import DSMC.Domain
import DSMC.Particles
import DSMC.Util
import DSMC.Util.Vector


-- | Cell contents with particles.
type CellContents = VU.Vector Particle


-- | Particles sorted by cells.
--
-- We store contents of all cells in a single densely packed unboxed
-- vector. Additionally cell count, cell starting positions in vector
-- (@s@) and cell sizes (@l@) are stored.
--
-- >   s1         s2    s3
-- >   |          |     |
-- > {[ooooooooo][oooo][oooooo]...}
-- >     cell1     c2     c3
-- >     l1=9      l2=4   l3=6
--
-- Note that any extra data about cells (like position or volume)
-- should be maintained separately from cell contents. We use this
-- approach because collision sampling and macroscopic parameter
-- calculation require different
data Cells = Cells !(VU.Vector Particle) !Int !(VU.Vector Int) !(VU.Vector Int)


-- | Assuming there's a linear ordering on all cells, Classifier must
-- yield index of cell for given particle.
type Classifier = Particle -> Int


-- | Fetch contents of n-th cell.
getCell :: Cells
        -> Int
        -- ^ Cell index.
        -> Maybe CellContents
getCell !(Cells ens _ starts lengths) !n =
    case (lengths VU.! n) of
      0 -> Nothing
      cl -> Just $ VU.slice (starts VU.! n) cl ens


-- | Map a function over cell indices and contents of every cell.
cellMap :: (Int -> Maybe CellContents -> a) -> Cells -> R.Array R.D R.DIM1 a
cellMap f !cells@(Cells _ l _ _) =
    R.fromFunction (R.ix1 $ l) (\(R.Z R.:. cellNumber) ->
                                    f cellNumber $! getCell cells cellNumber)


-- | Calculate cell numbers for particle ensemble.
classifyAll :: Monad m => Classifier -> Ensemble -> m (VU.Vector Int)
classifyAll classify ens = do
  classes' <- R.computeP $ R.map classify ens
  return $! R.toUnboxed classes'


-- | Sort particle ensemble into @N@ cells using the classifier
-- function.
--
-- Classifier's extent must match @N@, yielding numbers between @0@
-- and @N-1@.
sortParticles :: (Int, Classifier)
              -- ^ Cell count and classifier.
              -> Ensemble
              -> Cells
sortParticles (cellCount, classify) ens' = runST $ do
  classes <- classifyAll classify ens'

  let ens = R.toUnboxed ens'
      particleCount = VU.length ens

  -- Sequentially calculate particle indices inside cells and cell
  -- sizes.
  posns' <- VUM.replicate particleCount 0
  lengths' <- VUM.replicate cellCount 0
  iforM_ classes (\(particleNumber, cellNumber) -> do
       -- Increment cell particle count
       pos <- VUM.unsafeRead lengths' cellNumber
       VUM.unsafeWrite posns' particleNumber pos
       VUM.unsafeWrite lengths' cellNumber (pos + 1)
       return ())

  posns <- VU.unsafeFreeze posns'
  lengths <- VU.unsafeFreeze lengths'

  -- Starting positions for cells inside cell array
  let !starts = VU.prescanl' (+) 0 lengths

  -- Calculate indices for particles inside sorted grand vector of
  -- cell contents (inverse mapping index)
  sortedIds' <- VUM.replicate particleCount 0
  iforM_ classes (\(particleNumber, cellNumber) -> do
       let i = (starts VU.! cellNumber) + (posns VU.! particleNumber)
       VUM.unsafeWrite sortedIds' i particleNumber
       return ())
  sortedIds <- VU.unsafeFreeze sortedIds'

  -- Fill the resulting array in parallel
  sortedEns <- R.computeP $
               R.fromFunction
                    (R.ix1 $ particleCount)
                    (\(R.Z R.:. position) ->
                           ens VU.! (sortedIds VU.! position))

  return $! Cells (R.toUnboxed sortedEns) cellCount starts lengths



-- | Domain divided in regular grid with given steps by X, Y and Z
-- axes.
type RegularSubdivision = (Domain, Double, Double, Double)


-- | Return grid cell count and classifier for regular grid over
-- domain with given spatial steps.
makeRegularClassifier :: RegularSubdivision
                      -> (Int, Classifier)
makeRegularClassifier (d@(Domain xmin _ ymin _ zmin _), hx, hy, hz) =
    (xsteps * ysteps * zsteps, classify)
    where
        (w, l, h) = getDimensions d
        xsteps = ceiling $ w / hx
        ysteps = ceiling $ l / hy
        zsteps = ceiling $ h / hz
        classify ((x, y, z), _) =
            let
                nx = floor $ (x - xmin) / hx
                ny = floor $ (y - ymin) / hy
                nz = floor $ (z - zmin) / hz
            in
              nx + ny * xsteps + nz * xsteps * ysteps


-- | Return indexing function which maps cell numbers to central
-- points of regular cells.
makeRegularIndexer :: RegularSubdivision
                   -> (Int -> Point)
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
