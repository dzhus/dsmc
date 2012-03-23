-- | Raycasting traceable objects.

module DSMC.Traceables.Raycast
    ( Camera(..)
    , renderBodyPgm
    )

where

import Control.Applicative
import Data.Functor

import Data.Maybe

import DSMC.Particles
import DSMC.Traceables
import DSMC.Util
import qualified DSMC.Util.Vector as V


type Ray = Particle


-- | Observation point.
data Camera = Camera { position :: V.Point
                     -- ^ Absolute camera position
                     , direction :: V.Vector
                     -- ^ View direction
                     }


-- | Red, green, blue
data Triple a = Triple a a a
              deriving Show

instance Functor Triple where
    fmap f (Triple x y z) = Triple (f x) (f y) (f z)

instance Applicative Triple where
    pure f = Triple f f f
    (Triple f g h) <*> (Triple x y z) = Triple (f x) (g y) (h z)

type Color = Triple Double


-- | Preset colors
red, green, blue, white, cyan, magenta, yellow :: Color
red = Triple 1 0 0
green = Triple 0 1 0
blue = Triple 0 0 1
white = Triple 1 1 1

cyan = mixColors green blue
magenta = mixColors red blue
yellow = mixColors red green


scaleColor :: Color -> Double -> Color
scaleColor color f = clampColor $ pure (*f) <*> color


-- | Set color channels to be within range (0.0, 1.0) each.
clampColor :: Color -> Color
clampColor color = clamp <$> color
    where clamp x = max 0.0 (min 1.0 x)


mixColors :: Color -> Color -> Color
mixColors c1 c2 = clampColor (liftA2 (+) c1 c2)


-- | Generate numX * numY rays starting from camera plane parallel to
-- plane normal. Viewport dimensions are scaled.
spawnRays :: Camera
          -> Int        -- ^ Number of rays by horizontal axis
          -> Int        -- ^ Number of rays by vertical axis
          -> Double     -- ^ Viewport scale
          -> [Ray]
spawnRays (Camera p d) numX numY scale =
    let
        (n, sX, sY) = V.buildCartesian d
        xSteps = [-(numX `div` 2) .. (numX - 1) `div` 2]
        ySteps = [-(numY `div` 2) .. (numY - 1) `div` 2]
    in
      [Particle (p
                 V.<+> (sX V.*> (fromIntegral x) V.*> scale)
                 V.<+> (sY V.*> (fromIntegral y) V.*> scale)) n
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
            scaleColor red ((V.reverse n) V.<*> (velocity ray))

-- | Format color for use in PGM.
pgmColor :: Color -> String
pgmColor color =
    let
      rescale = pure (round . (* 255))
      (Triple r g b) = (pure show) <*> (rescale <*> color)
    in
      unwords [r, g, b]

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
