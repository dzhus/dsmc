import DSMC
import Render
import Vector

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GL

vectorGL :: Vector.Vector -> GL.Vector3 GLfloat
vectorGL (x, y, z) = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

vertexGL :: Vector.Point -> GL.Vertex3 GLfloat
vertexGL (x, y, z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

objectGL :: DSMC.Object -> IO ()
objectGL (DSMC.Sphere c r) = 
    preservingMatrix $ 
    do
      translate (vectorGL c)
      renderObject Solid (Sphere' (realToFrac r) 50 50)

objectGL (DSMC.Plane n d) = 
    let
        (v, sX, sY) = buildCartesian n
        p = n *> d
        iF = 100
    in
      do
        renderPrimitive Polygon $ do
          vertex $ (vertexGL (p <+> (sX *> iF) <+> (sY *> iF)))
          vertex $ (vertexGL (p <+> (sX *> iF) <-> (sY *> iF)))
          vertex $ (vertexGL (p <-> (sX *> iF) <-> (sY *> iF)))
          vertex $ (vertexGL (p <-> (sX *> iF) <+> (sY *> iF)))

bodyGL :: DSMC.Body -> IO ()
bodyGL (DSMC.Primitive b) = 
    do
      objectGL b

noColor = Color4 Disabled Disabled Disabled Disabled
fullColor = Color4 Enabled Enabled Enabled Enabled

-- Display intersection of two spheres
display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer, StencilBuffer]

  -- Plot a target z-surface
  colorMask $= noColor
  cullFace $= Just Back
  depthMask $= Enabled
  stencilMask $= 0
  bodyGL (DSMC.Primitive (DSMC.Sphere (-1, 0, 0) 2))

  -- Mark points of z-surface which are inside the object being
  -- clipped against with 1's
  depthMask $= Disabled
  stencilMask $= 1
  stencilTest $= Enabled
  stencilFunc $= (Always, 0, 0)
  stencilOp $= (OpKeep, OpKeep, OpInvert)
  bodyGL (DSMC.Primitive (DSMC.Sphere (1, 0.5, 0) 2))

  -- Now actually draw those fragments of the original surface which
  -- are in the body
  colorMask $= fullColor
  cullFace $= Just Back
  depthMask $= Enabled
  stencilFunc $= (Equal, 1, 1)
  bodyGL (DSMC.Primitive (DSMC.Sphere (-1, 0, 0) 2))

  swapBuffers

initfn :: IO ()
initfn = 
    let 
        light0 = Light 0
        lightColor = Color4 0.1 1.0 1.0 0.2
        lightAmbient = Color4 0.5 0.0 0.0 1.0
        lightPosition = Vertex4 1.0 1.0 1.0 0.0
    in
      do specular light0 $= lightColor
         GL.position light0 $= lightPosition
         lighting $= Enabled
         light light0 $= Enabled

         depthFunc $= Just Lequal

         matrixMode $= Projection
         perspective 40.0 1.0 1.0 20.0
         matrixMode $= Modelview 0
         lookAt (Vertex3 0.0 0.0 12.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

inputCb key state mod pos =
    case key of
      (MouseButton WheelUp) -> 
          do
            scale (1.1::GLfloat) 1.1 1.1
            postRedisplay Nothing
      (MouseButton WheelDown) -> 
          do
            scale (0.9::GLfloat) 0.9 0.9
            postRedisplay Nothing
      (SpecialKey KeyUp) ->
          do
            rotate 5 (vectorGL (1, 0, 0))
            postRedisplay Nothing
      (SpecialKey KeyDown) ->
          do
            rotate (-5) (vectorGL (1, 0, 0))
            postRedisplay Nothing
      (SpecialKey KeyLeft) ->
          do
            rotate 5 (vectorGL (0, 1, 0))
            postRedisplay Nothing
      (SpecialKey KeyRight) ->
          do
            rotate (-5) (vectorGL (0, 1, 0))
            postRedisplay Nothing
      _ -> return ()
  

main :: IO ()
main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer, WithStencilBuffer]
  createWindow "CSG viewer"
  displayCallback $= display
  keyboardMouseCallback $= Just inputCb
  initfn
  mainLoop