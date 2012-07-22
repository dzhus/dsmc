{-# LANGUAGE BangPatterns #-}

{-|

Particle tracking for regular spatial grid for DSMC.

This module is used to sort particles into ordered vector of cells for
collision step or macroscopic parameter sampling. We do not provide
any special cell datatype since it varies which cell data is required
on every step, so only particles in every cell are stored.

-}

module DSMC.Cells
    ( -- * Generic functions
      Cells(..)
    , getCell
    , CellContents
    , Classifier
    , sortParticles
    -- * Regular subdivision
    , RegularSubdivision
    , makeRegularClassifier
    , makeRegularIndexer
    )

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Control.Monad.ST

import Data.Strict.Maybe

import qualified Data.Array.Repa as R

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import DSMC.Domain
import DSMC.Particles
import DSMC.Util.Vector


-- | Cell contents with particles.
type CellContents = VU.Vector Particle


-- | Particles sorted by cells, along with cell starting positions in
-- vector and cell sizes.
data Cells = Cells (VU.Vector Particle) (VU.Vector Int) (VU.Vector Int)


-- | Assuming there's a linear ordering on all cells, Classifier must
-- yield index of cell for given particle.
type Classifier = Particle -> Int


-- | Fetch contents of n-th cell.
--
-- Bounds (in case cell index is invalid) are checked only on vector
-- level.
getCell :: Cells 
        -> Int
        -- ^ Cell index.
        -> Maybe CellContents
getCell !(Cells ens starts lengths) n =
    case (lengths VU.! n) of
      0 -> Nothing
      l -> Just $ VU.slice (starts VU.! n) l ens


-- | Calculate cell numbers for particle ensemble.
classifyAll :: Monad m => Classifier -> Ensemble -> m (VU.Vector Int)
classifyAll classify ens = do
  classes' <- R.computeP $ R.map classify ens
  return $! R.toUnboxed classes'


iforM_ :: (Monad m, VUM.Unbox a) =>
          VU.Vector a
       -> ((Int, a) -> m b)
       -> m ()
iforM_ v = VU.forM_ (VU.imap (,) v)


-- | Sort particle ensemble into @N@ cells using the classifier
-- function.
--
-- Classifier extent must match @N@, yielding numbers between @0@ and
-- @N-1@.
sortParticles :: (Int, Classifier)
              -- ^ Cell count and classifier.
              -> Ensemble
              -> Cells
sortParticles (cellCount, classify) ens' = runST $ do
  classes <- classifyAll classify ens'

  let ens = R.toUnboxed ens'
      particleCount = VU.length ens

  -- Cell sizes
  lengths' <- VUM.replicate cellCount 0

  -- Positions of particles inside cells
  posns' <- VUM.replicate particleCount 0

  -- Sequentially calculate particle indices inside cells and cell
  -- lengths.
  iforM_ classes (\(particleNumber, cellNumber) -> do
       -- Increment cell particle count
       count <- VUM.unsafeRead lengths' cellNumber
       VUM.unsafeWrite posns' particleNumber count
       VUM.unsafeWrite lengths' cellNumber (count + 1)
       return ())

  lengths <- VU.unsafeFreeze lengths'
  posns <- VU.unsafeFreeze posns'

  -- Starting positions for cells inside cell array
  let !starts = VU.prescanl' (+) 0 lengths

  -- Calculate indices for particles inside sorted grand vector of
  -- cell contents
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

  return $! Cells (R.toUnboxed sortedEns) starts lengths



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
