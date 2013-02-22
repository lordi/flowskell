module Flowskell.Lib.Shaders where
import Prelude hiding ( sum )
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Foldable ( Foldable, sum )
import Data.IORef
import Graphics.UI.GLUT

import Language.Scheme.Types
import Flowskell.State (State(..))
import Flowskell.SchemeUtils (extractFloat)
import Flowskell.ShaderUtils (readCompileAndLink)

loadShader :: IORef [Maybe Program] -> [LispVal] -> IO LispVal
loadShader shdLstRef [String vert, String frag] = do 
                        shdLst <- get shdLstRef
                        print "x"
                        prg <- readCompileAndLink vert frag
                        writeIORef shdLstRef (shdLst ++ [Just prg])
                        return $ Number (fromIntegral (length shdLst))
loadShader shdLstRef [String name] = do 
                        loadShader shdLstRef [String vert, String frag]
                        where vert = "shaders/" ++ name ++ ".vert"
                              frag = "shaders/" ++ name ++ ".frag"

setShader :: IORef [Maybe Program] -> [LispVal] -> IO LispVal
--setShader shdLstRef [Number n, List uniforms@[String _, _]] = do
--                        shdLst <- get shdLstRef
--                        -- TODO: check bounds
--                        let prg = shdLst !! (fromIntegral n)
--                            setUniform var val = do
--                              location <- get (uniformLocation prg var)
--                              reportErrors
--                              uniform location $= val
--                        setUniform "amt" (Index1 0.2)
--                        currentProgram $= prg
--                        return (Number 1)
setShader shdLstRef [Number n] = do
                        shdLst <- get shdLstRef
                        -- TODO: check bounds
                        currentProgram $= shdLst !! (fromIntegral n)
                        return (Number 1)
setShader shdLstRef [] = setShader shdLstRef [Number 0]

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
        ("blur", doBlur state)
      ]

