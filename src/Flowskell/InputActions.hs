module Flowskell.InputActions (
    keyboardMouseHandler,
    mouseHandler,
    motionHandler,
    actionReloadSource) where
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.UI.GLUT hiding (Bool, Float)
import Language.Scheme.Core (evalString)
import Flowskell.State
import qualified Flowskell.InputLine as IL
import Flowskell.TextureUtils (writeTextureToFile)

-- |Reload scheme source by initialising a new environment and storing it in
--  envRef.
actionReloadSource state = do
  Just env <- get $ environment state
  Just initFunc' <- get $ initFunc state
  putStrLn $ "Notice: Reloading " ++ (source state) 
  newEnv <- initFunc' (source state)
  environment state $= Just newEnv
  lastEnvironment state $= Just env

-- |Save last rendered screen texture to PNG file
actionScreenshot state = do
  let shotFilename = "flowskell-shot.png"
  Just fbTexture <- get $ lastRenderTexture state
  writeTextureToFile fbTexture shotFilename
  putStrLn $ "Notice: Saved screenshot to " ++ shotFilename

-- |Reset top level rotation (still incorrect)
actionResetView state = do
  matrixMode $= Projection
  -- This is redundant and also incorrect.
  -- TODO:
  -- either save aspect or (width, height) or
  -- original transform matrix in state
  loadIdentity
  let fov = 60
      near = 0.01
      far = 100
      aspect = 1
  perspective fov aspect near far
  translate $ Vector3 0 0 (-1::GLfloat)

actionModifyInputLine fnc state = do
  (get $ replInputLine state) >>= \x -> replInputLine state $= fnc x

actionToggle stateVar state = do
  (get $ stateVar state) >>= \x -> stateVar state $= not x

actionEvalInputLine state = do
  il <- get $ replInputLine state
  env <- (get $ environment state) >>= return . fromJust
  lines <- get $ replLines state
  let inputString = IL.getInput il
  result <- evalString env inputString
  replLines state $= [result, ">>> " ++ inputString] ++ lines
  replInputLine state $= IL.newInputLine

-- Input line bindings
keyboardAct (Char '\b') = actionModifyInputLine IL.backspace
keyboardAct (Char '\DEL') = actionModifyInputLine IL.del
keyboardAct (Char '\r') = actionEvalInputLine
keyboardAct (Char c) = actionModifyInputLine (IL.input c)
keyboardAct (SpecialKey KeyLeft) = actionModifyInputLine IL.left
keyboardAct (SpecialKey KeyRight) = actionModifyInputLine IL.right
keyboardAct (SpecialKey KeyHome) = actionModifyInputLine IL.pos1
keyboardAct (SpecialKey KeyEnd) = actionModifyInputLine IL.end

-- Function key bindings
keyboardAct (SpecialKey KeyF1) = actionToggle showHelp
keyboardAct (SpecialKey KeyF2) = actionToggle showREPL
keyboardAct (SpecialKey KeyF3) = actionToggle showFramesPerSecond
keyboardAct (SpecialKey KeyF5) = actionReloadSource
keyboardAct (SpecialKey KeyF6) = actionResetView
keyboardAct (SpecialKey KeyF7) = actionScreenshot
keyboardAct _ = \_ -> return ()

keyboardMouseHandler state (MouseButton WheelDown) Down _ _ = do
  let s = 0.9 :: GLfloat
  matrixMode $= Projection
  scale s s s

keyboardMouseHandler state (MouseButton WheelUp) Down _ _ = do
  let s = 1.1 :: GLfloat
  matrixMode $= Projection
  scale s s s

keyboardMouseHandler state (MouseButton _) Down mod position =
  lastPosition state $= (Position (-1) (-1))

keyboardMouseHandler state key st mod position =
  when (st == Down) $ keyboardAct key state

mouseHandler state keystate mod pos@(Position x y) =
  lastPosition state $= (Position (-1) (-1))

motionHandler :: State -> MotionCallback
motionHandler state pos@(Position x y) = do
  postRedisplay Nothing
  Position xt yt <- get (lastPosition state)
  lastPosition state $= pos
  when (xt /= -1 || yt /= -1) $ do
     let Vector3 xl yl _ = Vector3 (fromIntegral (x - xt)) (fromIntegral (y - yt)) 0
     matrixMode $= Projection
     rotate (yl / 10.0) (Vector3 (-1) 0 (0 :: GLfloat))
     rotate (xl / 10.0) (Vector3 0 (-1) (0 :: GLfloat))

