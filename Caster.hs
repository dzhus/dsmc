import DSMC
import Render

body = Intersection
       (Intersection 
       (Union 
        (Primitive (Sphere (0, 0, 0) 2))
        (Intersection 
         (Primitive (Sphere (0, 0, 0) 4))
         (Primitive (Plane (0, 0, 1) 0)))) 
       (Complement 
        (Primitive (Sphere (-2, 0, -1) 3))))
       (Complement 
        (Primitive (Sphere (3, 0, 0) 1)))

camera = Camera (-1, 0, -1.0) (-15)
x = 300
y = 300
scale = 0.03

main = do
  putStrLn (renderBodyPgm camera body x y scale)