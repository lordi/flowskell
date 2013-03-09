module Flowskell.Shot where
import Graphics.UI.GLUT hiding (Bool, Float)
import System.Exit

import Flowskell.TextureUtils (writeTextureToFile)
import Flowskell.State
import Flowskell.Interpreter (initSchemeEnv, initPrimitives)
import Flowskell.Display (initDisplay, reshapeHandler, displayHandler)

shot = do
  let resolution = Size 512 512
      numberOfFrames = 15

  (_, [filename, pngname]) <- getArgsAndInitialize
  state <- makeState filename
  forceResolution state $= Just resolution
  initDisplay state

  extraPrimitives <- initPrimitives state
  let initFunc' = initSchemeEnv extraPrimitives
  initFunc state $= Just initFunc'
  env <- initFunc' filename
  environment state $= Just env
  idleCallback $= Just (idle state pngname numberOfFrames)
  displayCallback $= displayHandler state
  reshapeCallback $= Just (reshapeHandler state)
  mainLoop

idle state pngname 0 = do
  Just texture <- get $ lastRenderTexture state
  writeTextureToFile texture pngname
  exitWith ExitSuccess

idle state pngname countdown = do
  idleCallback $= Just (idle state pngname (countdown - 1))
  postRedisplay Nothing
