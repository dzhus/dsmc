module Main

where

import Data.List
import Data.List.Utils

import DSMC
import Particles
import Traceables
import Vector

plotParticle :: Particle -> String
plotParticle (Particle (x, y, z) (vx, vy, vz)) = join " " (map show [x, y, z, vx, vy, vz])

exportParticles :: [Particle] -> IO ()
exportParticles particles = do
     mapM_ (putStrLn . plotParticle) particles

fillBody b = clipBody b [Particle (x / 10, y / 10, z / 10) (0, 0, 0) 
                             | x <- [-50..50], y <- [-50..50], z <- [-50..50]]

main =
    let
        plane1 = Primitive (Plane (0, 0, 1) 0)
        plane2 = Primitive (Plane (0, 0, -1) (-1))
        plane3 = Primitive (Plane (0, 1, 1) 0)
        sphere1 = Primitive (Sphere (0, 0, 0) 4)
        sphere2 = Primitive (Sphere (0, 0, -2) 3)
        sphere3 = Primitive (Sphere (4, 0, 1.02) 1.2)
        body = Intersection [(Union [(Intersection [plane1, plane2, Complement sphere1]), sphere2]), plane3]
        dt = 0.05
        tmax = 2.8
        particles = [Particle (x / 5, y / 5, 10) (0, 0, -5) | x <- [-20..20], y <- [-20..20]]
    in do
       mapM_ exportParticles (unfoldr (\(t, pcs) -> if t > 0 
                                                    then 
                                                        let
                                                            pNew = processParticles pcs dt body
                                                        in
                                                          Just (pNew, (t - dt, pNew)) 
                                                    else Nothing) (tmax, particles))
