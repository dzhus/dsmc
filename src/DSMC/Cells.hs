{-# LANGUAGE BangPatterns #-}

{-|

Particle tracking for spatial grid for DSMC.

This module is used to sort (classify) particles into ordered vector
of cells for collision step or macroscopic parameter sampling. We do
not provide any special cell datatype since it varies which cell data
is required on every step, so only particles in every cell are stored.

Monad is provided for storing grid options during the whole program
run.

-}

module DSMC.Cells
    ( -- * Generic functions for cells
      Cells
    , CellContents
    , getCell
    , cellMap
    -- * Particle tracking
    , Classifier
    , classifyParticles
    -- * Grids
    , Grid(..)
    , makeUniformClassifier
    , makeUniformIndexer
    -- * Monadic interface
    , GridMonad
    , GridWares(..)
    , runGrid
    , cellVolumes
    )

where

import Prelude hiding (Just, Nothing, Maybe)

import Control.Monad.ST
import Control.Monad.Trans.Reader

import Data.Strict.Maybe

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V

import Control.Parallel.Stochastic

import DSMC.Domain
import DSMC.Particles
import DSMC.Traceables
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
data Cells = Cells !CellContents !Int !(VU.Vector Int) !(VU.Vector Int)


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
{-# INLINE getCell #-}


-- | Map a function over cell indices and contents of every cell.
cellMap :: (Int -> Maybe CellContents -> a) -> Cells -> R.Array R.D R.DIM1 a
cellMap f !cells@(Cells _ l _ _) =
    R.fromFunction (R.ix1 $ l) (\(R.Z R.:. cellNumber) ->
                                    f cellNumber $! getCell cells cellNumber)


-- | Calculate cell numbers for particle ensemble.
classifyAll :: Classifier -> Ensemble -> (VU.Vector Int)
classifyAll classify ens = runST $ do
  classes' <- R.computeP $ R.map classify ens
  return $ R.toUnboxed classes'


-- | Classify particle ensemble into @N@ cells using the classifier
-- function.
--
-- Classifier's extent must match @N@, yielding numbers between @0@
-- and @N-1@.
classifyParticles :: (Int, Classifier)
              -- ^ Cell count and classifier.
              -> Ensemble
              -> Cells
classifyParticles (cellCount, classify) ens' = runST $ do
  let ens = R.toUnboxed ens'
      particleCount = VU.length ens
      classes = classifyAll classify ens'

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
  let starts = VU.prescanl' (+) 0 lengths

  -- Calculate indices for particles inside classified grand vector of
  -- cell contents (inverse mapping index)
  classifiedIds' <- VUM.replicate particleCount 0
  iforM_ classes (\(particleNumber, cellNumber) -> do
       let i = (starts VU.! cellNumber) + (posns VU.! particleNumber)
       VUM.unsafeWrite classifiedIds' i particleNumber
       return ())
  classifiedIds <- VU.unsafeFreeze classifiedIds'

  -- Fill the resulting array in parallel
  classifiedEns <- R.computeP $
               R.fromFunction
                    (R.ix1 $ particleCount)
                    (\(R.Z R.:. position) ->
                           ens VU.! (classifiedIds VU.! position))

  return $ Cells (R.toUnboxed classifiedEns) cellCount starts lengths


-- | Domain divided in uniform grid with given steps by X, Y and Z
-- axes.
data Grid = UniformGrid !Domain !Double !Double !Double
            deriving Show


-- | Return grid cell count and classifier for a grid.
makeUniformClassifier :: Grid -> (Int, Classifier)
makeUniformClassifier (UniformGrid d@(Domain xmin _ ymin _ zmin _) hx hy hz) =
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


-- | Function which maps cell numbers to central points of uniform
-- cells.
type Indexer = Int -> Point


-- | Return indexer for a grid.
makeUniformIndexer :: Grid -> Indexer
makeUniformIndexer (UniformGrid d@(Domain xmin _ ymin _ zmin _) hx hy hz) =
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


-- | Build vector of domains corresponding to cells of grid.
gridDomains :: Grid -> V.Vector Domain
gridDomains g@(UniformGrid _ hx hy hz) =
    let
        ixer = makeUniformIndexer g
        (count, _) = makeUniformClassifier g
    in runST $ do
       doms <- R.computeP $ R.fromFunction (R.ix1 $ count)
           (\(R.Z R.:. cellNumber) ->
                makeDomain (ixer cellNumber) hx hy hz)
       return $ R.toVector doms


-- | Calculate volumes of grid cells wrt body within the domain. For
-- every cell, 'freeVolume' is called with the domain of cell.
-- Calculation is performed in parallel.
--
-- Since our grid are static, this is usually done only once when the
-- grid is first defined. We throw away the used seeds.
cellVolumes :: ParallelSeeds
            -- ^ One-use seeds for cut cell volume approximation.
            -> Grid 
            -> Body 
            -> Int
            -> (VU.Vector Double)
cellVolumes seeds grid b testPoints =
  fst $
  splitParMapST (freeVolumes b testPoints)
  (gridDomains grid) seeds


-- | Monad used to keep grid options and cell volumes. Due to the
-- low-level 'Cells' structure we use to store particles sorted in
-- cells, things may break badly if improper/inconsistent
-- classifier/indexer parameters are used with cells structure. It
-- also helps to maintain precalculated cell volumes. See
-- 'MacroSamplingMonad'.
type GridMonad = ReaderT GridWares DSMCRootMonad


-- | Data stored in 'GridMonad'.
data GridWares =
    GridWares { classifier :: (Int, Classifier)
              -- ^ Cell count and classifier function.
              , indexer :: Int -> Point
              , volumes :: !(VU.Vector Double)
              -- ^ Vector of cell volumes.
              }


-- | Run action using spatial subdivision.
runGrid :: GridMonad a 
        -> ParallelSeeds 
        -- ^ One-use seeds used for
        -> Grid
        -> Body
        -- ^ Body within the domain of the grid.
        -> Int
        -- ^ Use that many points to approximate every cell volume.
        -> DSMCRootMonad a
runGrid r seeds grid b testPoints =
    runReaderT r $
    GridWares
    (makeUniformClassifier grid)
    (makeUniformIndexer grid)
    (cellVolumes seeds grid b testPoints)
