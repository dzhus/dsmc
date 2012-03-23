{-|
  
Standalone raycasting runner for traceable objects.
  
|-}

import Traceables
import Raycast

main = 
    let
        plane1 = plane (-1, 0, -1) 0
        sphere1 = sphere (0, 0, 0) 2
        sphere2 = sphere (0, 0, 1) 2
        sphere3 = sphere (1, 0, 1) 4
        body = intersection [sphere1, sphere2, plane1]
        camera = Camera (5, 5, 1) (-1, -1, -0.3)
        x = 600
        y = 600
        scale = 1/100
    in
      do
        putStrLn (renderBodyPgm camera body x y scale)

