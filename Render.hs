module Render

where

import DSMC
import Vector

type Ray = Particle

-- Camera orientation and distance from (0, 0, 0)
type Camera = (Vector, Double)

-- Generate numX * numY rays starting from camera plane parallel to
-- plane normal. Viewport dimensions are scaled.
spawnRays :: Camera -> Int -> Int -> Double -> [Ray]
spawnRays cam@(v,d) numX numY scale =
    let
        (n, sX, sY) = buildCartesian v
        c = v *> d
        xSteps = [-(numX `div` 2) .. (numX - 1) `div` 2]
        ySteps = [-(numY `div` 2) .. (numY - 1) `div` 2]
    in
      [Particle (c 
                 <+> (sX *> (fromIntegral x) *> scale) 
                 <+> (sY *> (fromIntegral y) *> scale)) n
       | x <- xSteps, y <- ySteps]