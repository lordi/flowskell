module Flowskell.Viewer where
import Control.Monad (when)
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.UI.GLUT hiding (Bool, Float)
import Flowskell.Interpreter (initSchemeEnv, evalFrame)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (getModificationTime)

#ifdef USE_JACK
import Flowskell.Lib.Jack (initJack)
#endif
#ifdef USE_TEXTURES
import Flowskell.Lib.Textures (initTextures)
#endif
#ifdef RENDER_TO_TEXTURE
import Flowskell.Lib.Shaders (initShaders)
#endif

import Flowskell.TextureUtils
import Flowskell.ShaderUtils
import Flowskell.State
import Flowskell.InputActions (
    actionReloadSource, motionHandler, mouseHandler,
    keyboardMouseHandler)
import Flowskell.Display (
    initDisplay, reshapeHandler, displayHandler)

viewer = do
  (_, [filename]) <- getArgsAndInitialize
  state <- makeState filename
  initDisplay state

#ifdef RENDER_TO_TEXTURE
  shaderIOPrimitives <- initShaders state
#else
  shaderIOPrimitives <- return []
#endif

#ifdef USE_TEXTURES
  texturesIOPrimitives <- initTextures state
#else
  texturesIOPrimitives <- return []
#endif

#ifdef USE_JACK
  jackIOPrimitives <- initJack
#else
  jackIOPrimitives <- return []
#endif

  let extraPrimitives = jackIOPrimitives ++ texturesIOPrimitives ++ shaderIOPrimitives
      initFunc' = initSchemeEnv extraPrimitives
  initFunc state $= Just initFunc'
  env <- initFunc' filename
  environment state $= Just env
  idleCallback $= Just (idle state)

  -- Flowskell.Display
  displayCallback $= displayHandler state
  reshapeCallback $= Just (reshapeHandler state)

  -- Flowskell.InputActions
  motionCallback $= Just (motionHandler state)
  mouseCallback $= Just (mouseHandler state)
  keyboardMouseCallback $= Just (keyboardMouseHandler state)

  -- GLUT main loop
  mainLoop


-- |Idle function. Check modifcation date of the current source file
--  and reload the environment when it has changed.
idle state = do
  now <- getCurrentTime
  lastCheckTime <- get $ lastReloadCheck state
  lastFPSTime <- get $ lastFrameCounterTime state
  when (diffUTCTime now lastCheckTime > 0.5) $ do
    Just env <- get $ environment state
    modTime <- getModificationTime (source state)
    lastModTime <- get $ lastSourceModification state
    when (lastModTime < modTime) $ actionReloadSource state
    lastSourceModification state $= modTime
    lastReloadCheck state $= now
  when (diffUTCTime now lastFPSTime > 0.75) $ do
    cnt <- get $ frameCounter state
    framesPerSecond state $= (fromIntegral cnt / realToFrac (diffUTCTime now lastFPSTime))
    frameCounter state $= 0
    lastFrameCounterTime state $= now

  postRedisplay Nothing

