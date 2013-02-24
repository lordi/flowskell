module Flowskell.State where
import Data.IORef
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float)
import Flowskell.Interpreter (initSchemeEnv, evalFrame)
import Language.Scheme.Types (Env)

import Data.Time.Clock (getCurrentTime)
import qualified Data.Time.Clock as C

import Flowskell.TextureUtils
import Flowskell.ShaderUtils

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
    lastRenderDepthBuffer :: IORef (Maybe RenderbufferObject),
    lastPosition :: IORef Position,
    blurProgram :: IORef (Maybe Program),
    lastReloadCheck :: IORef C.UTCTime,
    lastSourceModification :: IORef C.UTCTime,
    source :: String,
    initFunc :: IORef (Maybe (String -> IO Env))
    }

makeState :: String -> IO State
makeState source' = do
    environment' <- newIORef Nothing
    rotation' <- newIORef (Vector3 0 0 (0 :: GLfloat))
    blurFactor' <- newIORef 0
    renderTexture' <- newIORef Nothing
    lastRenderTexture' <- newIORef Nothing
    renderFramebuffer' <- newIORef Nothing
    lastRenderFramebuffer' <- newIORef Nothing
    lastRenderDepthBuffer' <- newIORef Nothing
    blurProgram' <- newIORef Nothing
    depthBuffer' <- newIORef Nothing
    lastPosition' <- newIORef (Position (-1) (-1 :: GLint))
    lastReloadCheck' <- getCurrentTime >>= newIORef
    lastSourceModification' <- getCurrentTime >>= newIORef
    initFunc' <- newIORef Nothing
    return State {
        environment = environment',
        rotation = rotation',
        blurFactor = blurFactor',
        renderTexture = renderTexture',
        lastRenderTexture = lastRenderTexture',
        renderFramebuffer = renderFramebuffer',
        lastRenderFramebuffer = lastRenderFramebuffer',
        lastPosition = lastPosition',
        blurProgram = blurProgram',
        lastRenderDepthBuffer = lastRenderDepthBuffer',
        depthBuffer = depthBuffer',
        lastReloadCheck = lastReloadCheck',
        lastSourceModification = lastSourceModification',
        source = source',
        initFunc = initFunc'
        }
