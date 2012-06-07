{-# LANGUAGE BangPatterns #-}

{-|

Macroscopic parameters calculation.

We use regular spatial grid for sampling.

-}

module DSMC.Macroscopic
    ( MacroCell
    , Macroscopic
    , Velocity
    , samplingSort
    , sampleVels
    , printVels
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


-- | Sampling cell is attached to certain point in space and has a
-- number of specimen.
type MacroCell = (Point, CellContents)


-- | Macroscopic parameters are measured at certain point by averaging
-- over specimen.
type Macroscopic a = (Point, a)


-- | Flow velocity.
type Velocity = Macroscopic Vec3


type CellSampler a = MacroCell -> Maybe a


-- | Sample velocity inside a cell.
sampleVelocity :: CellSampler Velocity
sampleVelocity !(pt, ens) =
    case VU.null ens of
      True -> Nothing
      False -> Just $ (pt, (VU.foldl' (\v0 (_, v) -> v0 <+> v) (0, 0, 0) ens) .^ 
                             (1 / fromIntegral (VU.length ens)))


-- | Sort particles for further sampling.
samplingSort :: Int 
             -- ^ Grid size.
             -> Classifier
             -- ^ Classifier which matches the grid size.
             -> (Int -> Point)
             -- ^ Cell indexer.
             -> Ensemble -> V.Vector MacroCell
samplingSort cellCount cls ind ens =
    let
        points = V.generate cellCount ind
        conts = runST $ sortParticles cellCount cls ens
    in
      V.zip points conts


sampleVels :: Monad m => V.Vector MacroCell -> m (V.Vector (Maybe Velocity))
sampleVels cells =
    let
        arr = R.fromVector (R.ix1 $ V.length cells) cells
    in do
      e <- R.computeP $ R.map sampleVelocity arr
      return $ R.toVector e


printVels :: V.Vector (Maybe Velocity) -> IO ()
printVels samples =
    V.forM_ samples
         (\mb -> do case mb of
                      Nothing -> return ()
                      Just ((x, y, z), (u, v, w)) ->
                          putStrLn $ unwords (map show [x, y, z, u, v, w]))
