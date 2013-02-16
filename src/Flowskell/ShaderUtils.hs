module Flowskell.ShaderUtils where
import Prelude hiding ( sum )
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Foldable ( Foldable, sum )
import Data.IORef
import Graphics.UI.GLUT

-- Make sure that GLSL is supported by the driver, either directly by the core
-- or via an extension.
checkGLSLSupport :: IO ()
checkGLSLSupport = do
   version <- get (majorMinor glVersion)
   unless (version >= (2,0)) $ do
      extensions <- get glExtensions
      unless ("GL_ARB_shading_language_100" `elem` extensions) $
         ioError (userError "No GLSL support found.")

readAndCompileShader :: Shader s => FilePath -> IO s
readAndCompileShader filePath = do
   src <- readFile filePath
   [shader] <- genObjectNames 1
   shaderSource shader $= [src]
   compileShader shader
   reportErrors
   ok <- get (compileStatus shader)
   infoLog <- get (shaderInfoLog shader)
   mapM_ putStrLn ["Shader info log for '" ++ filePath ++ "':", infoLog, ""]
   unless ok $ do
      deleteObjectNames [shader]
      ioError (userError "shader compilation failed")
   return shader

linkShaders :: [VertexShader] -> [FragmentShader] -> IO (Program)
linkShaders vs fs = do
   [brickProg] <- genObjectNames 1
   attachedShaders brickProg $= (vs, fs)
   linkProgram brickProg
   reportErrors
   ok <- get (linkStatus brickProg)
   infoLog <- get (programInfoLog brickProg)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [brickProg]
      ioError (userError "linking failed")
   return brickProg

readCompileAndLink :: String -> String -> IO (Program)
readCompileAndLink vspath fspath = do
  vs <- readAndCompileShader vspath
  fs <- readAndCompileShader fspath
  linkShaders [vs] [fs]

installBrickShaders :: [VertexShader] -> [FragmentShader] -> IO ()
installBrickShaders vs fs = do
   [brickProg] <- genObjectNames 1
   attachedShaders brickProg $= (vs, fs)
   linkProgram brickProg
   reportErrors
   ok <- get (linkStatus brickProg)
   infoLog <- get (programInfoLog brickProg)
   mapM_ putStrLn ["Program info log:", infoLog, ""]
   unless ok $ do
      deleteObjectNames [brickProg]
      ioError (userError "linking failed")

   currentProgram $= Just brickProg

   let setUniform var val = do
       location <- get (uniformLocation brickProg var)
       reportErrors
       uniform location $= val

   setUniform "BrickColor" (Color3 1.0 0.3 (0.2 :: GLfloat))
   setUniform "MortarColor" (Color3 0.85 0.86 (0.84 :: GLfloat))
   setUniform "BrickSize" (Vertex2 0.30 (0.15 :: GLfloat))
   setUniform "BrickPct" (Vertex2 0.90 (0.85 :: GLfloat))
   setUniform "LightPosition" (Vertex3 0 0 (4 :: GLfloat))
