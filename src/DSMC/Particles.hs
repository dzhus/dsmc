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
    , printEnsemble
    , fromUnboxed1
    -- * Flows
    , Flow(..)
    , modelConcentration
    )

where

import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as VU

import DSMC.Types
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


-- | Calculate what model concentration will simulate real flow
-- concentration wrt statistical weight of single particle.
modelConcentration :: Flow -> Double
modelConcentration flow = (concentration flow) / (statWeight flow)


-- | Repa array of particles.
type Ensemble = R.Array R.U R.DIM1 Particle


-- | Convert between Repa 'R.DIM1'-arrays and unboxed 'VU.Vector's.
fromUnboxed1 :: VU.Vector Particle -> Ensemble
fromUnboxed1 v = R.fromUnboxed (R.ix1 $ VU.length v) v


emptyEnsemble :: Ensemble
emptyEnsemble = fromUnboxed1 $ VU.empty


-- | Amount of particles in ensemble.
ensembleSize :: Ensemble -> Int
ensembleSize ens = n where (R.Z R.:. n) = R.extent ens

-- | Print particles, one per row, using the format:
--
-- > x y z u v w
--
-- where @x y z@ are position coordinates and @u v w@ are velocity
-- components.
printEnsemble :: Ensemble -> IO ()
printEnsemble particles = do
  VU.forM_ (R.toUnboxed particles)
        (\((x, y, z), (u, v, w)) -> putStrLn $ unwords (map show [x, y, z, u, v, w]))
