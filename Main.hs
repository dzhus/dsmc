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
        p1 = Particle (0, 5, 0.5) (0, 1, -6)
        plane1 = Primitive (Plane (0, 0.3, 1) 0)
        plane2 = Primitive (Plane (0, -1, 1) 10)
        plane3 = Primitive (Plane (0, -0.3, -1) 3)
        body = Union plane3 (Union plane1 plane2)
        dt = 0.01
     in do
     exportParticles (unfoldr (\(t, p) -> if t > 0 then Just (hit (move dt p) body, (t - dt, hit (move dt p) body)) else Nothing) (3, p1))

