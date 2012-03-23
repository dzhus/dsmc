{-|

Macroscopic variables calculation.

-}

module DSMC.Macroscopic

where

import DSMC.Types
import DSMC.Util.Vector


-- | Cell contains a list of particles and is used to calculate a
-- macroscopic variable at single point of simulation domain.
data Cell = Cell Point [Particle]


-- | Macroscopic velocity.
data Velocity = Velocity Point Vector


-- | Sort particles in domain to a list of spherical cells with given
-- radius. Cells are centered at nodes regular grid with spatial step
-- (in each direction) equal to radius.
sphericalCells :: Domain 
              -> [Particle] 
              -- ^ All particles in domain.
              -> Double
              -- ^ Cell radius.
              -> [Cell]
sphericalCells (Box xmin xmax ymin ymax zmin zmax) particles radius =
    [let
        cx = xmin + x * radius
        cy = ymin + y * radius
        cz = zmin + z * radius
        c = (cx, cy, cz)
     in
       (Cell c (filter (\p -> (distance (position p) c) < radius) particles))
           | x <- [0 .. (xmax - xmin) / radius],
             y <- [0 .. (ymax - ymin) / radius],
             z <- [0 .. (zmax - zmin) / radius]]


-- | Calculate velocity for cell with particles.
sampleVelocity :: Cell -> Velocity
sampleVelocity (Cell point particles) =
    Velocity point (averageVelocity particles)


-- | Calculate average velocity for a list of particles.
averageVelocity :: [Particle] -> Vector
averageVelocity particles =
    (foldl (<+>) (0, 0, 0) (map velocity particles)) *> (1 / fromIntegral (length particles))
