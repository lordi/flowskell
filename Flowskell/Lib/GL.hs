module Flowskell.Lib.GL where
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Control.Monad
import Control.Monad.Error
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float)
import Data.Time.Clock
import Data.Time.Calendar

import Language.Scheme.Types

intToGLfloat :: Float -> GLfloat
intToGLfloat x = realToFrac x

makeTeapot [] = do
    renderObject Solid (Teapot 1)
    return (Bool True)

doIdentity [] = loadIdentity >> return (Bool True)
doPush [] = glPushMatrix >> return (Bool True)
doPop [] = glPopMatrix >> return (Bool True)

doColor :: [LispVal] -> IO LispVal
doColor [Float r, Float g, Float b] = do
    color $ Color3 (togl r) (togl g) (togl b)
    materialDiffuse Front $= c
    materialAmbient Front $= c
    materialSpecular Front $= c
    return (Bool True)
        where togl = intToGLfloat . realToFrac
              c = Color4 (togl r) (togl g) (togl b) (0.2::GLfloat)

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

glIOPrimitives = [
                   ("make-cube", makeCube),
                   ("make-teapot", makeTeapot),

                   ("color", doColor),
                   ("scale", doScale),
                   ("translate", doTranslate),
                   ("rotate", doRotate),
                   ("identity", doIdentity),
                   ("push", doPush),
                   ("pop", doPop)
                 ]

