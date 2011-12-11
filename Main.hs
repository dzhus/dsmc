import Data.List
import Data.List.Utils

import DSMC

plotParticle :: Particle -> String
plotParticle (Particle (x, y, z) (vx, vy, vz)) = join " " (map show [x, y, z, vx, vy, vz])

exportParticles :: [Particle] -> IO ()
exportParticles particles = do
     mapM_ (putStrLn . plotParticle) particles

main =
     let
        plane1 = Primitive (Plane (0, -1, 1) (3))
        plane2 = Primitive (Plane (0, 0, 1) (-7))
        plane3 = Primitive (Plane (0, 1, 1) (3))
        sphere = Primitive (Sphere (0, 1, 0) 2)
        body = Union (Union plane1 plane3) (sphere)
        dt = 0.1
     in do
     mapM_ (\p1 -> exportParticles (unfoldr (\(t, p) -> if t > 0 then Just (hit (move dt p) body, (t - dt, hit (move dt p) body)) else Nothing) (3, p1))) [Particle (x / 5, y / 5, 5) (0, 0, -5) | y <- [-10..10], x <- [-10..10]]
