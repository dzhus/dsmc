{-# LANGUAGE BangPatterns #-}

{-|

Gas-surface interactions.

-}

module DSMC.Surface
    ( Collider
    , makeCll
    , makeSpecular
    )

where

import Control.Monad.ST

import System.Random.MWC

import DSMC.Util.Constants
import DSMC.Util.Vector

-- | A function which takes PRNG state, molecular velocity, surface
-- normal and samples post-collisional wrt to impregnable wall
-- boundary condition.
type Collider s = GenST s -> Vec3 -> Vec3 -> ST s Vec3


-- | Make a collider for Cercignani-Lampis model.
makeCll :: Double 
        -- ^ Body temperature.
        -> Double
        -- ^ Kinetic energy accomodation for normal velocity component.
        -> Double 
        -- ^ Accomodation for tangential momentum.
        -> Collider s
makeCll t alphanor sigmatan =
    let
        f = sqrt (2 * t * unigas)
        alphatan = sigmatan * (2 - sigmatan)
        cll :: Collider s
        cll !g !n !vel =
            let
                !e1 = normalize $ n >< vel
                !e2 = normalize $ n >< e1
                !ui = -(vel .* n)
                !vi = vel .* e2
                !urm = ui * sqrt (1 - alphanor) / f
                !vrm = vi * (1 - sigmatan)
            in do
              !angle <- uniformR (0.0, pi * 2) g
              !angle' <- uniformR (0.0, pi * 2) g
              !v2 <- uniform g
              !v2' <- uniform g
              let !r = sqrt (- (alphatan * log v2))
                  !vr = r * (cos angle) * f + vrm
                  !wr = r * (sin angle) * f
                  !r' = sqrt (- (alphanor * log v2'))
                  !ur = sqrt (urm * urm + 2 * r' * urm * (cos angle') + r' * r') * f

              return $! ((e1 .^ wr) <+> (e2 .^ vr) <+> (n .^ ur))
        {-# INLINE cll #-}
    in
      cll


-- | Make the specular reflection collider.
makeSpecular :: Collider s
makeSpecular _ !v !n = return $! v <-> (n .^ (v .* n) .^ 2)
