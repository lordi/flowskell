module Flowskell.Viewer where
import Control.Monad (when, forM_)
import Data.Maybe (isJust, fromJust)
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float)
import Flowskell.Interpreter (initSchemeEnv, evalFrame)
import Language.Scheme.Types (Env, LispVal (Atom, String))
import Language.Scheme.Core (evalString)
import Control.Concurrent
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Flowskell.Lib.GL (setColor) -- TODO move to GLUtils.hs

#ifdef USE_JACK
import Flowskell.Lib.Jack (initJack)
#endif
#ifdef USE_TEXTURES
import Flowskell.Lib.Textures (initTextures)
#endif
#ifdef RENDER_TO_TEXTURE
import Flowskell.Lib.Shaders (initShaders)
#endif

import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import System.Directory (getModificationTime)

import Flowskell.TextureUtils
import Flowskell.ShaderUtils
import Flowskell.State
import Flowskell.InputActions (actionReloadSource, motionHandler, mouseHandler, keyboardMouseHandler)
import Flowskell.Display (initDisplay, reshapeHandler, displayHandler)
import qualified Flowskell.InputLine as IL

viewer = do
  (progname, [filename]) <- getArgsAndInitialize
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

