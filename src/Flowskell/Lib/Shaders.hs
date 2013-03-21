module Flowskell.Lib.Shaders where
import Prelude hiding ( sum )
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Foldable ( Foldable, sum )
import Data.IORef
import Data.Maybe (fromJust)
import Data.Array (elems)
import Data.List (elemIndex)
import Graphics.UI.GLUT hiding (Float)

import Language.Scheme.Types
import Flowskell.State (State(..))
import Flowskell.SchemeUtils (extractFloat)
import Flowskell.ShaderUtils (readCompileAndLink)

extractGLfloat :: LispVal -> GLfloat
extractGLfloat = realToFrac . extractFloat

loadShader :: IORef [Maybe Program] -> [LispVal] -> IO LispVal
loadShader shdLstRef [String vert, String frag] = do 
                        shdLst <- get shdLstRef
                        prg <- readCompileAndLink vert frag
                        writeIORef shdLstRef (shdLst ++ [Just prg])
                        return $ Number (fromIntegral (length shdLst))
loadShader shdLstRef [String name] = do 
                        loadShader shdLstRef [String vert, String frag]
                        where vert = "shaders/" ++ name ++ ".vert"
                              frag = "shaders/" ++ name ++ ".frag"

setShader :: IORef [Maybe Program] -> [LispVal] -> IO LispVal
setShader shdLstRef [Number n] = do
                        shdLst <- get shdLstRef
                        -- TODO: check bounds
                        currentProgram $= shdLst !! (fromIntegral n)
                        return (Number 1)
setShader shdLstRef [] = setShader shdLstRef [Number 0]

getShader :: IORef [Maybe Program] -> [LispVal] -> IO LispVal
getShader shdLstRef [] = do
                        shdLst <- get shdLstRef
                        shd <- get $ currentProgram
                        let Just index = elemIndex shd shdLst
                        return (Number $ fromIntegral index)

setUniform shdLstRef [String name, f@(Float _)] = do
                        Just prg <- get currentProgram
                        let setUniform var val = do
                            location <- get (uniformLocation prg var)
                            reportErrors
                            uniform location $= val
                        setUniform name (Index1 ((extractFloat f) :: GLfloat))
                        return (Number 1)
setUniform shdLstRef [String name, (Vector v)] = do
                        Just prg <- get currentProgram
                        let [x, y, z] = map extractGLfloat (elems v)
                            setUniform var val = do
                              location <- get (uniformLocation prg var)
                              reportErrors
                              uniform location $= val
                        setUniform name (Vertex3 x y z)
                        return (Number 1)


doBlur :: State -> [LispVal] -> IO LispVal
doBlur state [n] = do
    blurFactor state $= (extractFloat n)
    return (Number 1)

initShaders :: State -> IO [(String, [LispVal] -> IO LispVal)]
initShaders state = do
    shdLstRef <- newIORef [Nothing]
    return  [
        ("load-shader", loadShader shdLstRef),
        ("shader", setShader shdLstRef),
        ("get-shader", getShader shdLstRef),
        ("set-uniform", setUniform shdLstRef),
        ("blur", doBlur state)
      ]

