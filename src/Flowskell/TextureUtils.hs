module Flowskell.TextureUtils where 
import Data.Vector.Storable (unsafeWith)
 
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D, Level, Border, TextureSize2D(..))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (Proxy(..), PixelInternalFormat(..))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (PixelData(..))

import System.Exit -- TODO remove
import Codec.Picture.Types
import Codec.Picture (readPng)
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization
import Graphics.UI.GLUT
import Data.Word
import Foreign.Marshal.Alloc

loadImage :: String -> IO ()
loadImage path = do 
    image <- readPng path
    case image of
     (Left s) -> do print s
                    exitWith (ExitFailure 1)
     (Right d) -> do image2Texture d

image2Texture (ImageRGB8 (Image width height dat)) =
  let size = (TextureSize2D (fromIntegral width) (fromIntegral height)) in
  unsafeWith dat $ \ptr ->
    texImage2D Nothing NoProxy 0 RGB8 size 0 (PixelData RGB UnsignedByte ptr)

image2Texture (ImageRGBA8 (Image width height dat)) =
  let size = (TextureSize2D (fromIntegral width) (fromIntegral height)) in
  unsafeWith dat $ \ptr ->
    texImage2D Nothing NoProxy 0 RGBA8 size 0 (PixelData RGBA UnsignedByte ptr)

getAndCreateTexture :: String -> IO (Maybe TextureObject)
getAndCreateTexture fileName = do
    [texName] <- genObjectNames 1  -- generate our texture.
    textureBinding Texture2D $= Just texName  -- make our new texture the current texture.
    textureFunction $= Decal
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texture Texture2D $= Enabled
    loadImage fileName   
    return (Just texName)
