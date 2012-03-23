-- | Macroscopic parameters calculation.

module DSMC.Macroscopic

where

import DSMC.Domain
import DSMC.Particles
import DSMC.Util.Vector

data Cell = Cell Point [Particle]

data Speed = Speed Point Vector

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

sampleSpeed :: Cell -> Speed
sampleSpeed (Cell point particles) =
    Speed point (averageSpeed particles)

-- Calculate average speed in a cell
averageSpeed :: [Particle] -> Vector
averageSpeed particles =
    (foldl (<+>) (0, 0, 0) (map speed particles)) *> (1 / fromIntegral (length particles))
