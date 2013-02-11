module Flowskell.Viewer where
import Control.Monad (when)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.UI.GLUT hiding (Bool, Float)
import Flowskell.Interpreter (initSchemeEnv, evalFrame)
import Language.Scheme.Core (evalLisp')
import Language.Scheme.Types (LispVal (Atom, String))
#ifdef USE_JACK
import Flowskell.Lib.Jack (initJack)
#endif
#ifdef USE_TEXTURES
import Flowskell.Lib.Textures (initTextures)
#endif

import Flowskell.TextureUtils (getAndCreateTexture, loadImage)

import Control.Concurrent
import Control.Monad hiding (forM_)

import Foreign ( withArray )


imageSize :: TextureSize2D
imageSize = TextureSize2D 64 64

withImage :: (PixelData (Color3 GLubyte) -> IO ()) -> IO ()
withImage act =
   withArray [ Color3 (s (sin ti)) (s (cos (2 * tj))) (s (cos (ti + tj))) |
               i <- [ 0 .. fromIntegral w - 1 ],
               let ti = 2 * pi * i / fromIntegral w,
               j <- [ 0 .. fromIntegral h - 1 ],
               let tj = 2 * pi * j / fromIntegral h ] $
   act . PixelData RGB UnsignedByte
   where (TextureSize2D w h) = imageSize
         s :: Double -> GLubyte
         s x = truncate (127 * (1 + x))

viewer = let light0 = Light 0 in do
  (progname, [filename]) <- getArgsAndInitialize

  -- Framebuffer test
  --fbo <- genObjectNames 1
  --let fbName = head fbo
  --bindFramebuffer Framebuffer $= fbName

  --exts <- get glExtensions
  --putStrLn $ show exts
  -- /Framebuffer test

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
  --cullFace $= Just Front

  angle <- newIORef (0.0::GLfloat)

#ifdef USE_JACK
  jackIOPrimitives <- initJack
#else
  jackIOPrimitives <- return []
#endif

#ifdef USE_TEXTURES
  texturesIOPrimitives <- initTextures
#else
  texturesIOPrimitives <- return []
#endif

  let extraPrimitives = jackIOPrimitives ++ texturesIOPrimitives
      initFunc = (initSchemeEnv extraPrimitives)
  env <- initFunc filename
  envRef <- newIORef env
  writeIORef envRef env
  displayCallback $= display angle envRef
  idleCallback $= Just idle
  reshapeCallback $= Just (reshape angle)
  keyboardMouseCallback $= Just (keyboardMouse angle envRef initFunc)
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

-- |Reload scheme source by initialising a new environment and storing it in
--  envRef.
keyboardAct a envRef reinitFunc (SpecialKey KeyF5) Down = do
  env <- get envRef
  evalLisp' env (Atom "*source*") >>= \x -> case x of
    Right (String source) -> (reinitFunc source) >>= writeIORef envRef
    _ -> return ()

keyboardAct _ _ _ _ _ = return ()

keyboardMouse angle envRef reinitFunc key state modifiers position = do
  keyboardAct angle envRef reinitFunc key state
