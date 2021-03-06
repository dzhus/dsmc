{-# LANGUAGE BangPatterns #-}

{-|

Particles, ensembles, flow parameters.

-}

module DSMC.Particles
    ( -- * Particles
      Particle
    , move
    -- ** Particle ensembles
    , Ensemble
    , emptyEnsemble
    , ensembleSize
    , filterEnsemble
    , printEnsemble
    -- * Flows
    , Flow(..)
    , modelConcentration
    )

where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

import DSMC.Util
import DSMC.Util.Vector


-- | Gas particle with position and velocity.
type Particle = (Point, Vec3)


-- | Linearly move particle for t time and update its position.
move :: Time -> Particle -> Particle
move !dt !(pos, v) = (pos <+> (v .^ dt), v)
{-# INLINE move #-}


-- | Flow with given concentration, temperature, mass of molecule and
-- macroscopic velocity.
data Flow = Flow { concentration :: !Double
                 , temperature :: !Double
                 , mass :: !Double
                 , velocity :: !Vec3
                 , statWeight :: !Double
                 -- ^ How many real particles a single simulator
                 -- represents.
                 }
            deriving (Show)


-- | Calculate model concentration to simulate real flow concentration
-- wrt statistical weight of single particle as set in flow options.
modelConcentration :: Flow -> Double
modelConcentration flow = (concentration flow) / (statWeight flow)


-- | Repa array of particles.
type Ensemble = R.Array R.U R.DIM1 Particle


-- | Ensemble with zero particles in it.
emptyEnsemble :: Ensemble
emptyEnsemble = fromUnboxed1 $ VU.empty


-- | Amount of particles in an ensemble.
ensembleSize :: Ensemble -> Int
ensembleSize ens = n where (R.Z R.:. n) = R.extent ens


-- | Print particles, one per row, using the format:
--
-- > x y z u v w
--
-- where @x y z@ are position coordinates and @u v w@ are velocity
-- components. This is handy for debugging purposes.
printEnsemble :: Ensemble -> IO ()
printEnsemble particles = do
  VU.forM_ (R.toUnboxed particles)
        (\((x, y, z), (u, v, w)) -> putStrLn $ unwords (map show [x, y, z, u, v, w]))


-- | Filter out those particles which do not satisfy the predicate.
filterEnsemble :: (Particle -> Bool) -> Ensemble -> Ensemble
filterEnsemble pred' ens =
    let
        (R.Z R.:. size) = R.extent ens
        getter :: Int -> Particle
        getter !i = (R.!) ens (R.ix1 i)
        {-# INLINE getter #-}
        predI :: Int -> Bool
        predI !i = pred' $ getter i
    in
      R.selectP predI getter size ens
