import DSMC
import Data.List.Utils

plotParticle :: Particle -> String
plotParticle (Particle (x, y, z) (vx, vy, vz)) = join " " (map show [x, y, z, vx, vy, vz])

exportParticles :: [Particle] -> IO ()
exportParticles particles = do
     mapM_ (putStrLn . plotParticle) particles

main =
     let
        p1 = Particle (0, 0, 1) (1, 1, -3/2)
        p2 = Particle (0, 0, 1) (-1, 1/2, -1)
        plane = Plane (0, 0.3, 1, 0)
     in do
        exportParticles (map (\t -> snd (hit (move t p1) plane)) (map (/10) [1..20]))
        exportParticles (map (\t -> snd (hit (move t p2) plane)) (map (/10) [1..20]))
