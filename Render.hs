module Render

where

import Data.Maybe

import DSMC
import Vector

type Ray = Particle

-- Camera orientation and distance from (0, 0, 0)
type Camera = (Vector, Double)

-- Red, green, blue
type Color = (Double, Double, Double)

-- Preset colors
red, green, blue, white :: Color
red = (1, 0, 0)
green = (0, 1, 0)
blue = (0, 0, 1)
white = (1, 1, 1)

scaleColor :: Color -> Double -> Color
scaleColor (r, g, b) f = (r * f, g * f, b * f)

-- Generate numX * numY rays starting from camera plane parallel to
-- plane normal. Viewport dimensions are scaled.
spawnRays :: Camera -> Int -> Int -> Double -> [Particle]
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

-- Calculate ray color
rayCast :: Particle -> Body -> Color
rayCast ray b = 
    let
        fullTrace = traceParticle ray b
        trace = intersect fullTrace [((0, Nothing), (infinityP, Nothing))]
    in
      if null trace
      then white
      else 
          let
              n = fromJust (snd (fst (head trace)))
          in
            scaleColor red ((Vector.reverse n) <*> (speed ray))

-- Write list of pixel colors to PGM string
makePgm :: Int -> Int -> [Color] -> String
makePgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255" ++ stringify(xs)
    where stringify [] = ""
          stringify ((r,g,b):xs) = "\n" ++ show (round (r * 255)) ++ " " 
                                   ++ show (round (g * 255)) ++ " " 
				   ++ show (round (b * 255)) ++ " " 
				   ++ stringify xs

renderBody :: Camera -> Body -> Int -> Int -> Double -> String
renderBody cam b x y scale = makePgm x y (map (\p -> rayCast p b) (spawnRays cam x y scale))
