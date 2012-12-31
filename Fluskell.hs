import Graphics.Rendering.OpenGL hiding (Bool)
import Control.Monad
import Control.Monad.Error
import Graphics.UI.GLUT hiding (Bool)
import Data.Time.Clock
import Data.Time.Calendar
import Interpreter

time = getCurrentTime >>= return . utctDayTime

intToGLfloat :: Float -> GLfloat
intToGLfloat x = realToFrac x

myPoints :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
myPoints t = map (\k -> (sin(2*pi*k/32)/2.0+cos(tx+k/15.0)/4.0,cos(k/32*pi*k/32)/2.0+cos(tx/4.0+k/15.0)/2.0,0.0)) [1..32]
        where tx = t / 1000.0

timeInSeconds [] = do x <- time; return $ Number $ floor $ realToFrac x
timeInMilliSeconds [] = do x <- time; return $ Number $ floor $ 1000 * realToFrac x

makeLine :: [LispVal] -> IO LispVal
makeLine [Number t] = do
    renderPrimitive Lines $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) (myPoints (intToGLfloat $ realToFrac t))
    return (Bool True)

doColor :: [LispVal] -> IO LispVal
doColor [Number r, Number g, Number b] = do
    color $ Color3 (togl r) (togl g) (togl b)
    return (Bool True)
        where togl x = ((intToGLfloat . realToFrac) x) / 255.0

doTranslate :: [LispVal] -> IO LispVal
doTranslate [Number r, Number g, Number b] = do
    translate $ Vector3 (togl r) (togl g) (togl b)
    return (Bool True)
        where togl x = ((intToGLfloat . realToFrac) x) / 255.0

doScale :: [LispVal] -> IO LispVal
doScale [Number x, Number y, Number z] = do
    scale (togl x) (togl y) (togl z)
    return (Bool True)
        where togl x = ((intToGLfloat . realToFrac) x) / 255.0

doRotate :: [LispVal] -> IO LispVal
doRotate [Number a, Number x, Number y, Number z] = do
    rotate (togl a) $ Vector3 (togl x) (togl y) (togl z)
    return (Bool True)
        where togl x = ((intToGLfloat . realToFrac) x) / 255.0

makeCube :: [LispVal] -> IO LispVal
makeCube [] = do
    renderPrimitive Quads $ do
        vertex $ Vertex3 w w w
        vertex $ Vertex3 w w (-w)
        vertex $ Vertex3 w (-w) (-w)
        vertex $ Vertex3 w (-w) w
        vertex $ Vertex3 w w w
        vertex $ Vertex3 w w (-w)
        vertex $ Vertex3 (-w) w (-w)
        vertex $ Vertex3 (-w) w w
        vertex $ Vertex3 w w w
        vertex $ Vertex3 w (-w) w
        vertex $ Vertex3 (-w) (-w) w
        vertex $ Vertex3 (-w) w w
        vertex $ Vertex3 (-w) w w
        vertex $ Vertex3 (-w) w (-w)
        vertex $ Vertex3 (-w) (-w) (-w)
        vertex $ Vertex3 (-w) (-w) w
        vertex $ Vertex3 w (-w) w
        vertex $ Vertex3 w (-w) (-w)
        vertex $ Vertex3 (-w) (-w) (-w)
        vertex $ Vertex3 (-w) (-w) w
        vertex $ Vertex3 w w (-w)
        vertex $ Vertex3 w (-w) (-w)
        vertex $ Vertex3 (-w) (-w) (-w)
        vertex $ Vertex3 (-w) w (-w)
    return (Bool True)
    where w = 1.0 :: GLfloat

makeThrowErrorFunc :: ([LispVal] -> IO LispVal) -> [LispVal] -> IOThrowsError LispVal
makeThrowErrorFunc f obj = do
    x <- liftIO $ f obj
    return x

-- TODO "secs" should be a variable, not a funcion?! or at least cached
fluxPrimitives = [ ("make-line", f makeLine),
                   ("make-cube", f makeCube),

                   ("color", f doColor),
                   ("scale", f doScale),
                   ("translate", f doTranslate),
                   ("rotate", f doRotate),

                   ("secs", f timeInSeconds),
                   ("msecs", f timeInMilliSeconds)
                 ]
                    where f = IOFunc . makeThrowErrorFunc

main = do 
  (progname, [filename]) <- getArgsAndInitialize
  source <- readFile filename
  createWindow progname
  displayCallback $= display source
  idleCallback $= Just idle
  mainLoop

display source = do 
  clear [ColorBuffer]
  -- TODO: the following line should not be done every frame
  env <- primitiveBindings >>= flip bindVars fluxPrimitives
  preservingMatrix $ evalString env $ source ++ "(every-frame)"
  flush

idle = do
  postRedisplay Nothing

