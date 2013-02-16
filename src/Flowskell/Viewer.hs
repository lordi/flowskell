module Flowskell.Viewer where
import Control.Monad (when)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float)
import Flowskell.Interpreter (initSchemeEnv, evalFrame)
import Language.Scheme.Core (evalLisp')
import Language.Scheme.Types (Env, LispVal (Atom, String))
#ifdef USE_JACK
import Flowskell.Lib.Jack (initJack)
#endif
#ifdef USE_TEXTURES
import Flowskell.Lib.Textures (initTextures)
#endif

import Graphics.Rendering.OpenGL.GL.Texturing.Environments

import Flowskell.TextureUtils
import Flowskell.ShaderUtils
import Control.Concurrent
import Control.Monad hiding (forM_)

import Foreign ( withArray )

-- State data
-- TODO: Try to implement HasGetter/HasSetter for MVar instead IORef (Maybe ...)
data State = State {
    rotation :: IORef (Vector3 GLfloat),
    environment :: IORef (Maybe Env),
    blurFactor :: IORef GLfloat,
    renderTexture :: IORef (Maybe TextureObject),
    renderFramebuffer :: IORef (Maybe FramebufferObject),
    lastRenderTexture :: IORef (Maybe TextureObject),
    lastRenderFramebuffer :: IORef (Maybe FramebufferObject),
    depthBuffer :: IORef (Maybe RenderbufferObject),
    blurProgram :: IORef (Maybe Program)
    }

makeState :: IO State
makeState = do
    environment' <- newIORef Nothing
    rotation' <- newIORef (Vector3 0 0 (0 :: GLfloat))
    blurFactor' <- newIORef 0.5
    renderTexture' <- newIORef Nothing
    lastRenderTexture' <- newIORef Nothing
    renderFramebuffer' <- newIORef Nothing
    lastRenderFramebuffer' <- newIORef Nothing
    blurProgram' <- newIORef Nothing
    depthBuffer' <- newIORef Nothing
    return $ State {
        environment = environment',
        rotation = rotation',
        blurFactor = blurFactor',
        renderTexture = renderTexture',
        lastRenderTexture = lastRenderTexture',
        renderFramebuffer = renderFramebuffer',
        lastRenderFramebuffer = lastRenderFramebuffer',
        blurProgram = blurProgram',
        depthBuffer = depthBuffer'
        }


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
  --cullFace $= Just Front

  state <- makeState

#ifdef USE_JACK
  jackIOPrimitives <- initJack
#else
  jackIOPrimitives <- return []
#endif

#ifdef RENDER_TO_TEXTURE
  -- Initialize "renderTexture"
  [fbo] <- genObjectNames 1
  (Just fbTexture) <- createBlankTexture (1, 1)
  bindFramebuffer Framebuffer $= fbo
  framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing fbTexture 0

  -- Initialize "lastRenderTexture"
  [fbo2] <- genObjectNames 1
  (Just fbTexture2) <- createBlankTexture (1, 1)
  bindFramebuffer Framebuffer $= fbo2
  framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing fbTexture2 0

  -- Initialize "depthBuffer"
  [drb] <- genObjectNames 1
  bindRenderbuffer Renderbuffer $= drb
  renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize 1 1)
  framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer drb

  -- Initialize blur shader
  checkGLSLSupport
  prg <- readCompileAndLink "shaders/fade.vert" "shaders/fade.frag"

  -- Store all in state
  renderTexture state $= Just fbTexture
  renderFramebuffer state $= Just fbo
  lastRenderTexture state $= Just fbTexture2
  lastRenderFramebuffer state $= Just fbo2
  depthBuffer state $= Just drb
  blurProgram state $= Just prg

  let lastFrameTextureObject = Just fbTexture2
#else
  let lastFrameTextureObject = Nothing
#endif

#ifdef USE_TEXTURES
  texturesIOPrimitives <- initTextures lastFrameTextureObject
#else
  texturesIOPrimitives <- return []
#endif

  let extraPrimitives = jackIOPrimitives ++ texturesIOPrimitives
      initFunc = (initSchemeEnv extraPrimitives)
  env <- initFunc filename
  environment state $= Just env
  displayCallback $= display state
  idleCallback $= Just idle
  reshapeCallback $= Just (reshape state)
  keyboardMouseCallback $= Just (keyboardMouse state initFunc)
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

  putStrLn $ "Notice: Resizing all texture buffers to " ++ (show s)
  setTextureSize fbTexture (TextureSize2D w h)
  setTextureSize fbTexture2 (TextureSize2D w h)
  bindRenderbuffer Renderbuffer $= drb
  renderbufferStorage Renderbuffer DepthComponent' (RenderbufferSize w h)
#endif

  matrixMode $= Modelview 0

display state = do
  Just env <- get $ environment state
#ifdef RENDER_TO_TEXTURE
  Just fbTexture <- get $ renderTexture state
  Just fbTexture2 <- get $ lastRenderTexture state
  Just fbo <- get $ renderFramebuffer state
  Just fbo2 <- get $ lastRenderFramebuffer state
  Just prg <- get $ blurProgram state
  blurF <- get $ blurFactor state
  bindFramebuffer Framebuffer $= fbo
#endif
  clear [ColorBuffer, DepthBuffer]

  -- viewport' <- get viewport

  textureBinding Texture2D $= Nothing
#ifdef RENDER_TO_TEXTURE
  matrixMode $= Projection
  glPushMatrix
  loadIdentity
  let fov = 60
      near = 0.01
      far = 100
      aspect = 1
  perspective fov aspect near far
#else
  loadIdentity
#endif
  translate $ Vector3 0 0 (-1::GLfloat)

  --angle' <- get angle
  preservingMatrix $ do
    -- rotate angle' $ Vector3 0 0 (1::GLfloat)
    evalFrame env

#ifdef RENDER_TO_TEXTURE
  textureBinding Texture2D $= Just fbTexture2
  matrixMode $= Modelview 0
  loadIdentity
  translate (Vector3 0 0 (-1 :: GLfloat))

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  currentProgram $= Just prg
  let setUniform var val = do
      location <- get (uniformLocation prg var)
      reportErrors
      uniform location $= val
  setUniform "amt" (Index1 blurF)

  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0.0     )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-1.0)      1.0    0.0     )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   1.0       1.0    0.0     )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )

  flush
  blend $= Disabled
  currentProgram $= Nothing
  textureFunction $= Replace

  bindFramebuffer Framebuffer $= fbo2
  clear [ColorBuffer, DepthBuffer]
  textureBinding Texture2D $= Just fbTexture
  matrixMode $= Modelview 0
  loadIdentity
  translate (Vector3 0 0 (-0.5 :: GLfloat))

  -- resolve overloading, not needed in "real" programs
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0.0     )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-1.0)      1.0    0.0     )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   1.0       1.0    0.0     )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
  flush

  bindFramebuffer Framebuffer $= defaultFramebufferObject
  matrixMode $= Projection
  glPopMatrix
  -- viewport $= viewport'
  textureBinding Texture2D $= Just fbTexture
  clear [ ColorBuffer, DepthBuffer ]
  matrixMode $= Modelview 0
  loadIdentity
  translate (Vector3 0 0 (-0.5 :: GLfloat))

  -- resolve overloading, not needed in "real" programs
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Quads $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0.0     )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-1.0)      1.0    0.0     )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   1.0       1.0    0.0     )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
#endif
  swapBuffers

idle = do
  postRedisplay Nothing

--keyboardAct a _ _ (SpecialKey KeyLeft) Down = do
--  a' <- get a
--  writeIORef a (a' + 5)

--keyboardAct state _ _ (SpecialKey KeyRight) Down = do
--  a' <- get a
--  writeIORef a (a' - 5)

-- |Reload scheme source by initialising a new environment and storing it in
--  envRef.
keyboardAct state reinitFunc (SpecialKey KeyF5) Down = do
  Just env <- get $ environment state
  evalLisp' env (Atom "*source*") >>= \x -> case x of
    Right (String source) -> (reinitFunc source) >>= (\e -> environment state $= Just e)
    _ -> return ()

keyboardAct _ _ _ _ = return ()

keyboardMouse st reinitFunc key state modifiers position = do
  keyboardAct st reinitFunc key state
