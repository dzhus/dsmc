import Data.List
import Data.List.Utils

import DSMC

plotParticle :: Particle -> String
plotParticle (Particle (x, y, z) (vx, vy, vz)) = join " " (map show [x, y, z, vx, vy, vz])

exportParticles :: [Particle] -> IO ()
exportParticles particles = do
     mapM_ (putStrLn . plotParticle) particles

processParticles :: [Particle] -> Time -> Body -> [Particle]
processParticles particles dt body =
    clipBody body (map (\p -> hit (move dt p) dt body) particles)

fillBody b = clipBody b [Particle (x / 10, y / 10, z / 10) (0, 0, 0) 
                             | x <- [-50..50], y <- [-50..50], z <- [-50..50]]

main =
     let
        plane1 = Primitive (Plane (1.5, 0, 1) 0)
        plane2 = Primitive (Plane (0, -1, 1) 0)
        plane3 = Primitive (Plane (-0.8, 0.0, 1) 10)
        sphere1 = Primitive (Sphere (2, 0, 0) 3)
        sphere2 = Primitive (Sphere (2.1, -1, -0) 3)
        body = (Union plane1 sphere1)
        dt = 0.05
        tmax = 5
        particles = [Particle (x / 2, y / 2, 10) (0, 0, -5) | x <- [-20..20], y <- [-20..20]]
     in do
--       exportParticles (fillBody body)

       mapM_ exportParticles (unfoldr (\(t, pcs) -> if t > 0 
                                                    then 
                                                        let
                                                            pNew = processParticles pcs dt body
                                                        in
                                                          Just (pNew, (t - dt, pNew)) 
                                                    else Nothing) (tmax, particles))
