import Graphics.Rendering.OpenGL hiding (Bool)
import Control.Monad
import Control.Monad.Error
import Graphics.UI.GLUT hiding (Bool)
import Data.Time.Clock
import Data.Time.Calendar
import Interpreter

--runIOThrows' :: IOThrowsError a -> IO a
--runIOThrows' action = runErrorT (trapError action) >>= return . extractValue


time = getCurrentTime >>= return . utctDayTime

intToGLfloat :: Float -> GLfloat
intToGLfloat x = realToFrac x

myPoints :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
myPoints t = map (\k -> (sin(2*pi*k/32)/2.0+cos(t+k/15.0)/4.0,cos(k/32*pi*k/32)/2.0+cos(t/4.0+k/15.0)/2.0,0.0)) [1..32]
        where tx = t / 2.0

--draw_cube = ...
--every_frame = do
--    mapM_ draw_cube [1..10]

main = do 
  (progname, _) <- getArgsAndInitialize
  --env <- runIOThrows' $ load "test.scm"
  createWindow "Hello World"
  displayCallback $= display
  idleCallback $= Just idle
  mainLoop
display = do 
  clear [ColorBuffer]
  t <- time
  renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) (myPoints (intToGLfloat (realToFrac t)))
  flush
idle = do
  postRedisplay Nothing

point :: [LispVal] -> IOThrowsError LispVal
point [obj] = do
    --liftIO $ renderPrimitive Points []
    liftIO $ putStrLn "xx"
    liftIO $ flush
    return $ Bool True
    --where x = 1.0 :: GLfloat
