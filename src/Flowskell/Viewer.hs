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
import qualified Flowskell.InputLine as IL

helpText = "Flowskell 0.0.9 " ++
    "| F1 Help | F2 REPL | F3 FPS | F5 Reload | F6 Reset view | F7 Screenshot"

viewer = let light0 = Light 0 in do
  (progname, [filename]) <- getArgsAndInitialize

  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
  createWindow progname

  state <- makeState filename

#ifdef RENDER_TO_TEXTURE
  -- Initialize "renderTexture"
  [fbo] <- genObjectNames 1
  (Just fbTexture) <- createBlankTexture (1, 1)
  bindFramebuffer Framebuffer $= fbo
  framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing fbTexture 0

  -- Initialize Depth Buffer" for renderTexture
  [drb] <- genObjectNames 1
  bindRenderbuffer Renderbuffer $= drb
  renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize 1 1)
  framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer drb

  -- Initialize "lastRenderTexture"
  [fbo2] <- genObjectNames 1
  (Just fbTexture2) <- createBlankTexture (1, 1)
  bindFramebuffer Framebuffer $= fbo2
  framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing fbTexture2 0

  -- Initialize Depth Buffer for lastRenderTexture
  [drb2] <- genObjectNames 1
  bindRenderbuffer Renderbuffer $= drb2
  renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize 1 1)
  framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer drb2

  -- Initialize blur shader
  checkGLSLSupport
  prg <- readCompileAndLink "shaders/fade.vert" "shaders/fade.frag"

  -- Store all in state
  renderTexture state $= Just fbTexture
  renderFramebuffer state $= Just fbo
  depthBuffer state $= Just drb
  lastRenderTexture state $= Just fbTexture2
  lastRenderFramebuffer state $= Just fbo2
  lastRenderDepthBuffer state $= Just drb2
  blurProgram state $= Just prg

  bindFramebuffer Framebuffer $= fbo
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
  -- cullFace $= Just Back

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

#ifdef DEBUG
  clearColor $= Color4 0.5 0.5 0.5 0.5
#else
  clearColor $= Color4 0 0 0 1
#endif

  let extraPrimitives = jackIOPrimitives ++ texturesIOPrimitives ++ shaderIOPrimitives
      initFunc' = initSchemeEnv extraPrimitives
  initFunc state $= Just initFunc'
  env <- initFunc' filename
  environment state $= Just env
  displayCallback $= display state
  idleCallback $= Just (idle state)
  reshapeCallback $= Just (reshape state)
  motionCallback $= Just (motion state)
  mouseCallback $= Just (mouse state)
  keyboardMouseCallback $= Just (keyboardMouse state)
  mainLoop

reshape state s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  let fov = 60
      near = 0.01
      far = 100
      aspect = (fromIntegral w) / (fromIntegral h)
  perspective fov aspect near far
  translate $ Vector3 0 0 (-1::GLfloat)

#ifdef RENDER_TO_TEXTURE
  -- We need to resize the framebuffer textures, because the window size
  -- might have changed. Unfortunately, this may take a while.
  -- TODO: find a faster way
  Just fbTexture <- get $ renderTexture state
  Just fbTexture2 <- get $ lastRenderTexture state
  Just drb <- get $ depthBuffer state
  Just drb2 <- get $ lastRenderDepthBuffer state

  putStrLn $ "Notice: Resizing all texture buffers to " ++ (show s)
  setTextureSize fbTexture (TextureSize2D w h)
  setTextureSize fbTexture2 (TextureSize2D w h)
  bindRenderbuffer Renderbuffer $= drb
  renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize w h)
  bindRenderbuffer Renderbuffer $= drb2
  renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize w h)
#endif

unitQuad = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1) (-1)   0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-1)   1    0 )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   1    1    0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   1  (-1)   0 )

unitFrame = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive LineStrip $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0  )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-1.0)      1.0    0  )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   1.0       1.0    0  )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   1.0     (-1.0)   0  )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0  )

display state = do
  Just env <- get $ environment state
  fc <- get $ frameCounter state
  frameCounter state $= fc + 1

#ifdef RENDER_TO_TEXTURE
  Just fbTexture <- get $ renderTexture state
  Just fbTexture2 <- get $ lastRenderTexture state
  Just fbo <- get $ renderFramebuffer state
  Just fbo2 <- get $ lastRenderFramebuffer state
  Just prg <- get $ blurProgram state
  Just drb <- get $ depthBuffer state
  blurF <- get $ blurFactor state
  bindFramebuffer Framebuffer $= fbo
  depthFunc $= Just Less

#endif
  clear [ColorBuffer, DepthBuffer]

  textureBinding Texture2D $= Nothing

  matrixMode $= Modelview 0
  loadIdentity
  translate $ Vector3 0 0 (-1::GLfloat)

  preservingMatrix $ do
    evalFrame env

#ifdef RENDER_TO_TEXTURE
  --
  -- Blend the last frame over the current scene, with fade factor
  --
  depthFunc $= Just Always
  matrixMode $= Projection
  glPushMatrix -- Save original matrix
  loadIdentity

  currentProgram $= Just prg
  let setUniform var val = do
      location <- get (uniformLocation prg var)
      reportErrors
      uniform location $= val
  setUniform "amt" (Index1 blurF)

  textureBinding Texture2D $= Just fbTexture2
  unitQuad

  currentProgram $= Nothing

  let onlyWhen stateVar sth = (get $ stateVar state) >>= \x -> when x sth
  let withTextMode sth = do
      textureBinding Texture2D $= Nothing
      matrixMode $= Modelview 0
      preservingMatrix $ do
          loadIdentity
          setColor [1,1,1,0.8]
          sth

  onlyWhen showFramesPerSecond $ do
      withTextMode $ do
          currentRasterPosition $= Vertex4 (-0.9) (-0.9) 0 (1::GLfloat)
          fps <- get $ framesPerSecond state
          renderString Fixed9By15 $ show (realToFrac (round (fps * 100.0)) / 100.0) ++ " FPS"

  onlyWhen showHelp $ do
      withTextMode $ do
          currentRasterPosition $= Vertex4 (-0.9) (0.9) 0 (1::GLfloat)
          renderString Fixed9By15 helpText

  onlyWhen showREPL $ do
      withTextMode $ do
          currentRasterPosition $= Vertex4 (-0.9) (0.8) 0 (1::GLfloat)
          lines <- get $ replLines state
          forM_ (zip [1..] (reverse (take 10 lines))) $ \ (i, l) -> do
              renderString Fixed9By15 l
              currentRasterPosition $= Vertex4 (-0.9) (0.8 - (fromIntegral i) * 0.1) 0 (1::GLfloat)
          il <- get $ replInputLine state
          renderString Fixed9By15 $ ">>> " ++ IL.showInputLine il

  textureFunction $= Replace
  flush

  --
  -- Now, render the just finished scene also to "last frame buffer"
  --
  bindFramebuffer Framebuffer $= fbo2
  clear [ColorBuffer, DepthBuffer]
  textureBinding Texture2D $= Just fbTexture
  unitQuad
  flush

#ifdef DEBUG
  -- Debug frame
  textureBinding Texture2D $= Nothing
  color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
  lineStipple $= Nothing
  lighting $= Disabled
  unitFrame
#endif

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  textureBinding Texture2D $= Just fbTexture
  matrixMode $= Projection
  loadIdentity
  let fov = 92
      near = 0.001
      far = 1000
      aspect = 1
  perspective fov aspect near far

  clear [ ColorBuffer, DepthBuffer ]
  depthFunc $= Just Always
  unitQuad

#ifdef DEBUG
  -- Debug frame
  textureBinding Texture2D $= Nothing
  color $ (Color4 1 1 1 (1 ::GLfloat))
  lineStipple $= Just (1, 0x1C47)
  lighting $= Disabled
  unitFrame
  lighting $= Enabled
#endif

  matrixMode $= Projection
  glPopMatrix
#endif

  swapBuffers

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

-- |Reload scheme source by initialising a new environment and storing it in
--  envRef.
actionReloadSource state = do
  Just env <- get $ environment state
  Just initFunc' <- get $ initFunc state
  putStrLn $ "Notice: Reloading " ++ (source state) 
  initFunc' (source state) >>= (\e -> environment state $= Just e)

-- |Save last rendered screen texture to PNG file
actionScreenshot state = do
  let shotFilename = "flowskell-shot.png"
  Just fbTexture <- get $ lastRenderTexture state
  writeTextureToFile fbTexture shotFilename
  putStrLn $ "Notice: Saved screenshot to " ++ shotFilename

-- |Reset top level rotation (still incorrect)
actionResetView state = do
  matrixMode $= Projection
  -- This is redundant and also incorrect.
  -- TODO:
  -- either save aspect or (width, height) or
  -- original transform matrix in state
  loadIdentity
  let fov = 60
      near = 0.01
      far = 100
      aspect = 1
  perspective fov aspect near far
  translate $ Vector3 0 0 (-1::GLfloat)

actionModifyInputLine fnc state = do
  (get $ replInputLine state) >>= \x -> replInputLine state $= fnc x

actionToggle stateVar state = do
  (get $ stateVar state) >>= \x -> stateVar state $= not x

actionEvalInputLine state = do
  il <- get $ replInputLine state
  env <- (get $ environment state) >>= return . fromJust
  lines <- get $ replLines state
  let inputString = IL.getInput il
  result <- evalString env inputString
  replLines state $= [result, ">>> " ++ inputString] ++ lines
  replInputLine state $= IL.newInputLine

-- Input line bindings
keyboardAct state (Char '\b') Down = actionModifyInputLine IL.backspace state
keyboardAct state (Char '\DEL') Down = actionModifyInputLine IL.del state
keyboardAct state (Char '\r') Down = actionEvalInputLine state
keyboardAct state (Char c) Down = actionModifyInputLine (IL.input c) state
keyboardAct state (SpecialKey KeyLeft) Down = actionModifyInputLine IL.left state
keyboardAct state (SpecialKey KeyRight) Down = actionModifyInputLine IL.right state
keyboardAct state (SpecialKey KeyHome) Down = actionModifyInputLine IL.pos1 state
keyboardAct state (SpecialKey KeyEnd) Down = actionModifyInputLine IL.end state

-- Function key bindings
keyboardAct state (SpecialKey KeyF1) Down = actionToggle showHelp state
keyboardAct state (SpecialKey KeyF2) Down = actionToggle showREPL state
keyboardAct state (SpecialKey KeyF3) Down = actionToggle showFramesPerSecond state
keyboardAct state (SpecialKey KeyF5) Down = actionReloadSource state
keyboardAct state (SpecialKey KeyF6) Down = actionResetView state
keyboardAct state (SpecialKey KeyF7) Down = actionScreenshot state
keyboardAct _ _ _ = return ()

keyboardMouse state key st modifiers position = do
  lastPosition state $= (Position (-1) (-1))
  keyboardAct state key st

mouse state keystate mod pos@(Position x y) = do
  lastPosition state $= (Position (-1) (-1))

motion :: State -> MotionCallback
motion state pos@(Position x y) = do
  postRedisplay Nothing
  Position xt yt <- get (lastPosition state)
  lastPosition state $= pos
  when (xt /= -1 || yt /= -1) $ do
     let Vector3 xl yl _ = Vector3 (fromIntegral (x - xt)) (fromIntegral (y - yt)) 0
     matrixMode $= Projection
     rotate (yl / 10.0) (Vector3 (-1) 0 (0 :: GLfloat))
     rotate (xl / 10.0) (Vector3 0 (-1) (0 :: GLfloat))

