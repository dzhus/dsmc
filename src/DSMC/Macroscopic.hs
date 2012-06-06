{-|

Macroscopic parameters calculation.

We use regular spatial grid for sampling.

-}

module DSMC.Macroscopic

where

import Prelude hiding (Just, Nothing, Maybe, fst)

import Data.Strict.Maybe

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import DSMC.Cells
import DSMC.Domain
import DSMC.Particles
import DSMC.Util.Vector


-- | Sampling cell is attached to certain point in space and has a
-- number of specimen.
data MacroCell = MacroCell !Point !CellContents


-- | Macroscopic parameters are measured at certain point by averaging
-- over specimen.
type Macroscopic a = (Point, a)


-- | Flow velocity.
type Velocity = Macroscopic Vec3


type Sampler a = MacroCell -> Maybe a


type MacroClassifier = Int -> Point


sampleVelocity :: Sampler Velocity
sampleVelocity (MacroCell pt ens) =
    case VU.null ens of
      True -> Nothing
      False -> Just $ (pt, VU.foldl' (\v0 (_, v) -> v0 <+> v) (0, 0, 0) ens)


-- samplingSort :: Ensemble -> V.Vector Cell
-- samplingSort ens = 


-- sampleCells :: Monad m => Ensemble -> Sampler -> m (
