import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Control.Monad
import Control.Monad.Error
import Graphics.Rendering.GLU.Raw
import Graphics.UI.GLUT hiding (Bool, Float)
import Data.Time.Clock
import Data.Time.Calendar

import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Parser
import Language.Scheme.Variables

time = getCurrentTime >>= return . utctDayTime

intToGLfloat :: Float -> GLfloat
intToGLfloat x = realToFrac x

runIOThrows' :: IOThrowsError String -> IO String
runIOThrows' action = runErrorT (trapError action) >>= return . extractValue

timeInSeconds [] = do x <- time; return $ Float $ realToFrac x
timeInMilliSeconds [] = do x <- time; return $ Float $ 1000 * (realToFrac x)

makeTeapot [] = do
    renderObject Solid (Teapot 1)
    return (Bool True)

doColor :: [LispVal] -> IO LispVal
doColor [Float r, Float g, Float b] = do
    color $ Color3 (togl r) (togl g) (togl b)
    return (Bool True)
        where togl = intToGLfloat . realToFrac

doTranslate :: [LispVal] -> IO LispVal
doTranslate [Float r, Float g, Float b] = do
    translate $ Vector3 (togl r) (togl g) (togl b)
    return (Bool True)
        where togl = intToGLfloat . realToFrac

doScale :: [LispVal] -> IO LispVal
doScale [Float x, Float y, Float z] = do
    scale (togl x) (togl y) (togl z)
    return (Bool True)
        where togl = intToGLfloat . realToFrac

doRotate :: [LispVal] -> IO LispVal
doRotate [Float a, Float x, Float y, Float z] = do
    rotate (togl a) $ Vector3 (togl x) (togl y) (togl z)
    return (Bool True)
        where togl = intToGLfloat . realToFrac

n :: [Normal3 GLfloat]
n = [(Normal3 (-1.0) 0.0 0.0),
     (Normal3 0.0 1.0 0.0),
     (Normal3 1.0 0.0 0.0),
     (Normal3 0.0 (-1.0) 0.0),
     (Normal3 0.0 0.0 1.0),
     (Normal3 0.0 0.0 (-1.0))]

faces :: [[Vertex3 GLfloat]]
faces = [[(v 0), (v 1), (v 2), (v 3)],
         [(v 3), (v 2), (v 6), (v 7)],
         [(v 7), (v 6), (v 5), (v 4)],
         [(v 4), (v 5), (v 1), (v 0)],
         [(v 5), (v 6), (v 2), (v 1)],
         [(v 7), (v 4), (v 0), (v 3)]]

v :: Int -> Vertex3 GLfloat
v x = Vertex3 v0 v1 v2
    where v0
              | x == 0 || x == 1 || x == 2 || x == 3 = -1
              | x == 4 || x == 5 || x == 6 || x == 7 = 1
          v1
              | x == 0 || x == 1 || x == 4 || x == 5 = -1
              | x == 2 || x == 3 || x == 6 || x == 7 = 1
          v2
              | x == 0 || x == 3 || x == 4 || x == 7 = 1
              | x == 1 || x == 2 || x == 5 || x == 6 = -1

makeCube :: [LispVal] -> IO LispVal
makeCube [] = let nfaces = zip n faces
          in do mapM (\(n, [v0, v1, v2, v3]) -> do
                        renderPrimitive Quads $ do
                          normal n
                          vertex v0
                          vertex v1
                          vertex v2
                          vertex v3) nfaces
                return $ Bool True

makeThrowErrorFunc :: ([LispVal] -> IO LispVal) -> [LispVal] -> IOThrowsError LispVal
makeThrowErrorFunc f obj = do
    x <- liftIO $ f obj
    return x

-- TODO "secs" should be a variable, not a funcion?! or at least cached
fluxPrimitives :: [ ((String, String), LispVal) ]
fluxPrimitives = map (\(n, f) -> (("v", n), IOFunc $ makeThrowErrorFunc f)) [
                   ("make-cube", makeCube),
                   ("make-teapot", makeTeapot),

                   ("color", doColor),
                   ("scale", doScale),
                   ("translate", doTranslate),
                   ("rotate", doRotate),

                   ("secs", timeInSeconds),
                   ("msecs", timeInMilliSeconds)
                 ] 

main = let light0 = Light 0 in do
  (progname, [filename]) <- getArgsAndInitialize
  source <- readFile filename
  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  createWindow progname

  ambient (Light 0) $= Color4 0.2 0.2 0.2 1
  diffuse (Light 0) $= Color4 1 1 1 0.6
  position (Light 0) $= Vertex4  0 0 3 0
  lightModelAmbient $= Color4 0.2 0.2 0.2 1
  lightModelLocalViewer $= Disabled

  frontFace $= CW
  lighting $= Enabled
  light (Light 0) $= Enabled
  autoNormal $= Enabled
  normalize $= Enabled
  depthFunc $= Just Less

  matrixMode $= Projection
  loadIdentity
  let near = 0
      far = 100
      right = 3
      top = 3
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0
  matrixMode $= Projection

  -- perspective 40.0 1.0 1.0 10.0

  matrixMode $= Modelview 0

  --lookAt (Vertex3 0.0 0.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)

  displayCallback $= display source
  idleCallback $= Just idle
  mainLoop

display source = do 
  clear [ColorBuffer, DepthBuffer]
  -- TODO: the following line should not be done every frame
  env <- primitiveBindings >>= (flip extendEnv fluxPrimitives)
  preservingMatrix $ do
    let exprs = extractValue $ readExprList $ source ++ "\n(every-frame)" in do
        forM exprs $ runIOThrows . liftM show . evalLisp env
        forM exprs $ putStrLn . show
  swapBuffers

idle = do
  postRedisplay Nothing

