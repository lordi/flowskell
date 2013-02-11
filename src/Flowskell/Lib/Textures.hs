module Flowskell.Lib.Textures where
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Control.Monad
import Control.Monad.Error
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix)
import Graphics.UI.GLUT hiding (Bool, Float)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Array
import Language.Scheme.Types

import Flowskell.SchemeUtils
import Flowskell.TextureUtils

import Data.IORef

loadTexture :: IORef [Maybe TextureObject] -> [LispVal] -> IO LispVal
loadTexture textureListRef [String s] = do 
                        textureList <- get textureListRef
                        maybeTextureObj <- getAndCreateTexture s
                        writeIORef textureListRef (textureList ++ [maybeTextureObj])
                        return $ Number (fromIntegral (length textureList))

setTexture :: IORef [Maybe TextureObject] -> [LispVal] -> IO LispVal
setTexture textureListRef [Number n] = do
                        textureList <- get textureListRef
                        textureBinding Texture2D $= textureList !! (fromIntegral n)
                        return (Number 1)

initTextures :: IO [(String, [LispVal] -> IO LispVal)]
initTextures = do
    textureListRef <- newIORef []
    print "textures initialised"
    return  [
        ("load-texture", loadTexture textureListRef),
        ("texture", setTexture textureListRef)
        ]

