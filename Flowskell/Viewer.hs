import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Control.Monad
import Control.Monad.Error
import Graphics.Rendering.GLU.Raw
import Graphics.UI.GLUT hiding (Bool, Float)

import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Parser
import Language.Scheme.Variables

import Flowskell.Lib.GL (glIOPrimitives)
import Flowskell.Lib.Random (randomIOPrimitives)
import Flowskell.Lib.Time (timeIOPrimitives)

makeThrowErrorFunc :: ([LispVal] -> IO LispVal) -> [LispVal] -> IOThrowsError LispVal
makeThrowErrorFunc f obj = do
    x <- liftIO $ f obj
    return x

otherPrimitives :: [ ((String, String), LispVal) ]
otherPrimitives = map (\(n, f) -> (("v", n), IOFunc $ makeThrowErrorFunc f)) $
                timeIOPrimitives ++ glIOPrimitives ++ randomIOPrimitives

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

  env <- primitiveBindings >>= flip extendEnv otherPrimitives

  let exprs = extractValue $ readExprList $ source in do
    forM exprs $ runIOThrows . liftM show . evalLisp env
    forM exprs $ putStrLn . show
  displayCallback $= display env
  idleCallback $= Just idle
  mainLoop

display env = do 
  clear [ColorBuffer, DepthBuffer]
  preservingMatrix $ evalString env "(every-frame)" >>= putStr
  swapBuffers

idle = do
  postRedisplay Nothing

