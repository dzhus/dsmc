module DSMC

where

import Control.Monad
import Data.Maybe

import Particles
import Traceables
import Util
import Vector

data Domain = Box Double Double Double Double Double Double
              deriving Show


inDomain :: Domain -> Particle -> Bool
inDomain (Box xmin xmax ymin ymax zmin zmax) (Particle (x, y, z) v) =
    xmax >= x && x >= xmin &&
    ymax >= y && y >= ymin &&
    zmax >= z && z >= zmin         

clipDomain domain particles = filter (inDomain domain) particles

-- Update particle position and velocity after specular hit given a
-- normalized normal vector of surface in hit point and time since hit
-- (not positive)
reflectSpecular :: Particle -> Vector -> Time -> Particle
reflectSpecular p n t =
    move (-1 * t) p{speed = v <-> (n *> (v <*> n) *> 2)}
    where
      v = speed p

-- A very small amount of time for which the particle is moved after
-- reflecting from body surface.
hitShift = 10e-10

-- | Particle after possible collision with body during timestep
hit :: Time -> Body -> Particle -> Particle
hit dt b p = 
    let
        justHit = [(((-dt), Nothing), (0, Nothing))]
        fullTrace = trace p b
        insideTrace = intersect fullTrace justHit
    in
      if null insideTrace
      then p
      else
          let
              hitPoint = fst (head insideTrace)
              t = fst hitPoint
              n = snd hitPoint
              particleAtHit = move t p
          in
            move hitShift (reflectSpecular particleAtHit (fromJust n) t)

clipBody body particles = filter (\p -> not (inside p body)) particles

-- | Collisionless flow simulation step
processParticles :: [Particle] -> Time -> Body -> [Particle]
processParticles particles dt body =
    clipBody body (map ((hit dt body) . (move dt)) particles)
