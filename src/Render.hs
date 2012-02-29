-- | Raycasting traceable objects.

module Render
    (Camera(..)
    )

where

import Control.Monad
import Data.Functor
import qualified Data.List.Utils as L
import Data.Maybe

import Particles
import Traceables
import Util
import Vector

type Ray = Particle

-- | Observation point.
data Camera = Camera { position :: Point
                     -- ^ Absolute camera position
                     , direction :: Vector
                     -- ^ View direction
                     }


-- | Red, green, blue
type Color = [Double]

-- | Preset colors
red, green, blue, white, cyan, magenta, yellow :: Color
red = [1, 0, 0]
green = [0, 1, 0]
blue = [0, 0, 1]
white = [1, 1, 1]

cyan = mixColors green blue
magenta = mixColors red blue
yellow = mixColors red green

scaleColor :: Color -> Double -> Color
scaleColor color f = clampColor $ (*f) <$> color

-- | Set color channels to be within range (0.0, 1.0) each.
clampColor :: Color -> Color
clampColor color = clamp <$> color where clamp x = max 0.0 (min 1.0 x)

mixColors :: Color -> Color -> Color
mixColors c1 c2 = clampColor (liftM2 (+) c1 c2)


-- | Generate numX * numY rays starting from camera plane parallel to
-- plane normal. Viewport dimensions are scaled.
spawnRays :: Camera
          -> Int        -- ^ Number of rays by horizontal axis
          -> Int        -- ^ Number of rays by vertical axis
          -> Double     -- ^ Viewport scale
          -> [Ray]
spawnRays (Camera p d) numX numY scale =
    let
        (n, sX, sY) = buildCartesian d
        xSteps = [-(numX `div` 2) .. (numX - 1) `div` 2]
        ySteps = Prelude.reverse [-(numY `div` 2) .. (numY - 1) `div` 2]
    in
      [Particle (p
                 <+> (sX *> (fromIntegral x) *> scale)
                 <+> (sY *> (fromIntegral y) *> scale)) n
       | y <- ySteps, x <- xSteps]

-- | Calculate color of ray.
rayCast :: Ray -> Body -> Color
rayCast ray b =
    let
        fullTrace = trace b ray
        hitTrace = intersectTraces fullTrace [((0, Nothing),
                                               (infinityP, Nothing))]
    in
      if null hitTrace
      then white
      else
          let
              n = fromJust (snd (fst (head hitTrace)))
          in
            scaleColor red ((Vector.reverse n) <*> (speed ray))

pgmColor :: Color -> String
pgmColor color = 
    L.join " " $ show <$> rescale <$> color
    where
      rescale :: Double -> Int
      rescale = round . (* 255)

-- | Write list of pixel colors to PGM string.
makePgm :: Int     -- ^ Image width
        -> Int     -- ^ Image height
        -> [Color] -- ^ Pixel colors, by rows of image
        -> String
makePgm width height colors = "P3\n" ++
                              show width ++ " " ++ show height ++
                              "\n255" ++ stringify(colors)
    where stringify [] = ""
          stringify (c:cs) = "\n" ++ pgmColor c ++ stringify cs

-- | Raycast the body and return string of image in PGM format.
renderBodyPgm :: Camera
              -> Body   -- ^ Body to raycast
              -> Int    -- ^ Image width
              -> Int    -- ^ Image height
              -> Double -- ^ Viewport scale
              -> String
renderBodyPgm cam b x y scale =
    makePgm x y (map (\p -> rayCast p b) (spawnRays cam x y scale))
