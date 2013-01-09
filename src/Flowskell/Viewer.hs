module Flowskell.Viewer where
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.UI.GLUT hiding (Bool, Float)
import Flowskell.Interpreter (initSchemeEnv, evalFrame)

viewer = let light0 = Light 0 in do
  (progname, [filename]) <- getArgsAndInitialize

  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow progname
  ambient light0 $= Color4 0.2 0.2 0.2 1
  diffuse light0 $= Color4 1 1 1 0.6
  position light0 $= Vertex4 0 0 3 0
  lightModelAmbient $= Color4 0.2 0.2 0.2 1
  lightModelLocalViewer $= Disabled
  materialShininess Front $= 0.0
  shadeModel $= Smooth
  frontFace $= CW
  lighting $= Enabled
  light light0 $= Enabled
  autoNormal $= Enabled
  normalize $= Enabled
  depthFunc $= Just Less
  angle <- newIORef (0.0::GLfloat)
  env <- initSchemeEnv filename
  envRef <- newIORef env
  writeIORef envRef env
  displayCallback $= display angle envRef
  idleCallback $= Just idle
  reshapeCallback $= Just (reshape angle)
  keyboardMouseCallback $= Just (keyboardMouse angle envRef filename)
  mainLoop

reshape angle s@(Size w h) = do
  viewport $= ((Position 0 0), s)
  matrixMode $= Projection
  loadIdentity
  let fov = 60
      near = 0.01
      far = 100
      aspect = (fromIntegral w) / (fromIntegral h)
  perspective fov aspect near far
  translate $ Vector3 0 0 (-1::GLfloat)

  matrixMode $= Modelview 0

display angle envRef = do
  clear [ColorBuffer, DepthBuffer]

  angle' <- get angle
  env <- get envRef
  preservingMatrix $ do
    rotate angle' $ Vector3 0 0 (1::GLfloat)
    evalFrame env
  swapBuffers

idle = do
  postRedisplay Nothing

keyboardAct a _ _ (SpecialKey KeyLeft) Down = do
  a' <- get a
  writeIORef a (a' + 5)

keyboardAct a _ _ (SpecialKey KeyRight) Down = do
  a' <- get a
  writeIORef a (a' - 5)

keyboardAct a envRef f (SpecialKey KeyF5) Down = do
  putStrLn "Reloading"
  env <- initSchemeEnv f
  writeIORef envRef env

keyboardAct _ _ _ _ _ = return ()

keyboardMouse angle e f key state modifiers position = do
  keyboardAct angle e f key state
