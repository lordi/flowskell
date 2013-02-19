module Flowskell.TextureUtils where
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (texImage2D, Level, Border, TextureSize2D(..))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (Proxy(..), PixelInternalFormat(..))
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (PixelData(..))

import System.Exit -- TODO remove
import Codec.Picture.Types
import Codec.Picture (readPng, writePng)
import Graphics.UI.GLUT
import Data.Word (Word8)
import Foreign ( withArray )
import Foreign.ForeignPtr
import Foreign.Storable (sizeOf)
import Data.Vector.Storable (unsafeFromForeignPtr, unsafeWith)

loadImage :: String -> IO ()
loadImage path = do 
    image <- readPng path
    case image of
     (Left s) -> do print s
                    exitWith (ExitFailure 1)
     (Right d) -> image2Texture d

image2Texture (ImageRGB8 (Image width height dat)) =
    let size = (TextureSize2D (fromIntegral width) (fromIntegral height)) in
    unsafeWith dat $ \ptr ->
        texImage2D Nothing NoProxy 0 RGB8 size 0 (PixelData RGB UnsignedByte ptr)

image2Texture (ImageRGBA8 (Image width height dat)) =
    let size = (TextureSize2D (fromIntegral width) (fromIntegral height)) in
    unsafeWith dat $ \ptr ->
        texImage2D Nothing NoProxy 0 RGBA8 size 0 (PixelData RGBA UnsignedByte ptr)

makeImage :: TextureSize2D -> (GLsizei -> GLsizei -> Color3 GLubyte)
               -> (PixelData (Color3 GLubyte) -> IO ()) -> IO ()
makeImage (TextureSize2D w h) f act =
   withArray [ f i j |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ] ] $
   act . PixelData RGB UnsignedByte

makeImageA :: TextureSize2D -> (GLsizei -> GLsizei -> Color4 GLubyte)
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
makeImageA (TextureSize2D w h) f act =
   withArray [ f i j |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ] ] $
   act . PixelData RGBA UnsignedByte

-- |Store a 2D texture as a PNG image on the disk
writeTextureToFile :: TextureObject -> String -> IO ()
writeTextureToFile texture filename = do
    textureBinding Texture2D $= Just texture
    s@(TextureSize2D w' h') <- get $ textureSize2D (Left Texture2D) 0
    let w = fromIntegral w'
        h = fromIntegral h'
        bufsize = w * h * 3 * sizeOf (undefined :: Word8)
    fp <- mallocForeignPtrBytes bufsize
    withForeignPtr fp $ \ p ->
        getTexImage (Left Texture2D) 0 (PixelData RGB UnsignedByte p)
    let v = (unsafeFromForeignPtr fp 0 bufsize)
    writePng filename (Image w h v :: Image PixelRGB8)

createBlankTexture :: (Int, Int) -> IO (Maybe TextureObject)
createBlankTexture (width, height) =
    let size = (TextureSize2D (fromIntegral width) (fromIntegral height)) in do
    [texName] <- genObjectNames 1
    textureBinding Texture2D $= Just texName
    textureFunction $= Decal
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texture Texture2D $= Enabled
    setTextureSize texName size
    return (Just texName)

setTextureSize texName size = do
    -- TODO: This probably can be optimized
    textureBinding Texture2D $= Just texName
    makeImageA size (\i _ -> Color4 0 0 0 0) $
      texImage2D Nothing NoProxy 0 RGBA8 size 0

getAndCreateTexture :: String -> IO (Maybe TextureObject)
getAndCreateTexture fileName = do
    -- generate our texture name:
    [texName] <- genObjectNames 1
    -- make our new texture the current texture:
    textureBinding Texture2D $= Just texName  
    textureFunction $= Decal
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texture Texture2D $= Enabled
    loadImage fileName   
    return (Just texName)
