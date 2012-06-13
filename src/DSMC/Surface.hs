{-# LANGUAGE BangPatterns #-}

{-|

Gas-surface interactions.

-}

module DSMC.Surface
    ( Reflector
    , Surface(..)
    , makeReflector
    )

where

import Control.Monad.ST

import System.Random.MWC
import System.Random.MWC.Distributions (normal)

import DSMC.Util.Constants
import DSMC.Util.Vector


-- | A function which takes PRNG state, molecular velocity, surface
-- normal and samples post-collisional wrt to impregnable wall
-- boundary condition.
type Reflector s = GenST s -> Vec3 -> Vec3 -> ST s Vec3


data Surface = CLL { bodyTemperature :: !Double
                   -- ^ Absolute temperature of surface.
                   , alpha :: !Double
                   -- ^ Kinetic energy accomodation for normal velocity component.
                   , sigma :: !Double
                   -- ^ Accomodation for tangential momentum.
                   } |
               -- ^ Cercignani-Lampis-Lord model.
               Diffuse { bodyTemperature :: !Double
                       -- ^ Absolute temperature of surface.
                       , mass :: !Double
                       -- ^ Mass of reflected molecules (equal to
                       -- that in incident flow).
                       } |
               Mirror
               -- ^ Surface with specular reflection.


makeReflector :: Surface -> Reflector s
makeReflector (CLL t alphanor sigmatan) =
    let
        f = sqrt (2 * t * unigas)
        alphatan = sigmatan * (2 - sigmatan)
        cll :: Reflector s
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

makeReflector Mirror =  \_ !v !n -> return $! v <-> (n .^ (v .* n) .^ 2)

makeReflector (Diffuse t m) =
    let
        s = sqrt $ boltzmann * t / m
        beta = 1 / (s * (sqrt 2))
        diffuse :: Reflector s
        diffuse g n vel =
            let
                e1 = normalize $ n >< vel
                e2 = normalize $ n >< e1
            in do
              w <- normal 0 s g
              v <- normal 0 s g
              u' <- uniform g
              let u = (sqrt (- (log u'))) / beta
              return $ (e1 .^ v) <+> (e2 .^ w) <+> (n .^ u)
        {-# INLINE diffuse #-}
    in
      diffuse
