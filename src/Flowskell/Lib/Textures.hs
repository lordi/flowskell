module Flowskell.Lib.Textures where
import Graphics.Rendering.OpenGL hiding (Bool, Float)
import Graphics.Rendering.GLU.Raw
import Graphics.UI.GLUT hiding (Bool, Float)
import Data.Array
import Data.IORef
import Language.Scheme.Types

import Flowskell.TextureUtils (getAndCreateTexture)

loadTexture :: IORef [Maybe TextureObject] -> [LispVal] -> IO LispVal
loadTexture txtLstRef [String s] = do 
                        txtLst <- get txtLstRef
                        maybeTextureObj <- getAndCreateTexture s
                        writeIORef txtLstRef (txtLst ++ [maybeTextureObj])
                        return $ Number (fromIntegral (length txtLst))

setTexture :: IORef [Maybe TextureObject] -> [LispVal] -> IO LispVal
setTexture txtLstRef [Number n] = do
                        txtLst <- get txtLstRef
                        -- TODO: check bounds
                        textureBinding Texture2D $= txtLst !! (fromIntegral n)
                        return (Number 1)
setTexture txtLstRef [] = setTexture txtLstRef [Number 0]

initTextures :: TextureObject -> IO [(String, [LispVal] -> IO LispVal)]
initTextures lastFrameTO = do
    txtLstRef <- newIORef [Nothing, Just lastFrameTO]
    return  [
        ("load-texture", loadTexture txtLstRef),
        ("texture", setTexture txtLstRef)
        ]

