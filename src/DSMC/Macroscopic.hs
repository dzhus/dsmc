-- | Macroscopic parameters calculation.

module DSMC.Macroscopic

where

import DSMC.Domain
import DSMC.Particles
import DSMC.Util.Vector

data Cell = Cell Point [Particle]

data Velocity = Velocity Point Vector

circularCells :: Domain -> [Particle] -> Double -> [Cell]
circularCells (Box xmin xmax ymin ymax zmin zmax) particles radius =
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

sampleVelocity :: Cell -> Velocity
sampleVelocity (Cell point particles) =
    Velocity point (averageVelocity particles)

-- Calculate average velocity in a cell
averageVelocity :: [Particle] -> Vector
averageVelocity particles =
    (foldl (<+>) (0, 0, 0) (map velocity particles)) *> (1 / fromIntegral (length particles))
