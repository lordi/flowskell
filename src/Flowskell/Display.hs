module Flowskell.Display (
    reshapeHandler, displayHandler, initDisplay
    ) where
import Control.Monad (when, forM_)
import Data.Maybe (isJust, fromJust)
import Data.IORef
import Graphics.UI.GLUT hiding (Bool, Float)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix) -- TODO check if these are really needed
import Flowskell.Interpreter (initSchemeEnv, evalFrame)
import Control.Concurrent
import Flowskell.Lib.GL (setColor) -- TODO move to GLUtils.hs

import Flowskell.TextureUtils
import Flowskell.ShaderUtils
import Flowskell.State
import qualified Flowskell.InputLine as IL
import qualified Flowskell.Constants as C

initDisplay state = do
  let light0 = Light 0

  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
  createWindow C.programNameWithVersion

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

#endif
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
reshapeHandler state size = do
  maybeForceRes <- get $ forceResolution state
  currentRes <- get $ currentResolution state

  let s@(Size w h) = maybe size id maybeForceRes
  when (s /= currentRes) $ do
      currentResolution state $= s
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
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (-1) (-1)   0 )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (-1)   1    0 )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1    1    0 )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1  (-1)   0 )

unitFrame = do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive LineStrip $ do
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0  )
    texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-1.0)      1.0    0  )
    texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   1.0       1.0    0  )
    texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   1.0     (-1.0)   0  )
    texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-1.0)    (-1.0)   0  )

displayHandler state = do
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
    evalFrame state env

#ifdef RENDER_TO_TEXTURE
  --
  -- Blend the last frame over the current scene, with fade factor
  --
  depthFunc $= Just Always
  matrixMode $= Projection
  glPushMatrix -- Save original matrix
  loadIdentity

  oldPrg <- get currentProgram
  currentProgram $= Just prg
  let setUniform var val = do
      location <- get (uniformLocation prg var)
      reportErrors
      uniform location $= val
  setUniform "amt" (Index1 blurF)

  textureBinding Texture2D $= Just fbTexture2
  preservingMatrix $ do
    scale (-1) ((1) ::GLfloat) 1
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
          renderString Fixed9By15 C.helpText

  onlyWhen showREPL $ do
      withTextMode $ do
          currentRasterPosition $= Vertex4 (-0.9) (0.8) 0 (1::GLfloat)
          lines <- get $ replLines state
          forM_ (zip [1..] (reverse (take 10 lines))) $ \ (i, l) -> do
              renderString Fixed9By15 l
              currentRasterPosition $= Vertex4 (-0.9) (0.8 - (fromIntegral i) * 0.1) 0 (1::GLfloat)
          il <- get $ replInputLine state
          renderString Fixed9By15 $ C.prompt ++ IL.showInputLine il

  textureFunction $= Replace
  flush

  --
  -- Now, render the just finished scene also to "last frame buffer"
  --
  bindFramebuffer Framebuffer $= fbo2
  clear [ColorBuffer, DepthBuffer]
  textureBinding Texture2D $= Just fbTexture
  preservingMatrix $ do
    scale (-1) ((1) ::GLfloat) 1
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
  preservingMatrix $ do
    scale (-1) ((-1) ::GLfloat) 1
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

  currentProgram $= oldPrg
  swapBuffers
