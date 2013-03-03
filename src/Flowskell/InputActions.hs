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
  initFunc' (source state) >>= (\e -> environment state $= Just e)

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
keyboardAct state (Char '\b') Down = actionModifyInputLine IL.backspace state
keyboardAct state (Char '\DEL') Down = actionModifyInputLine IL.del state
keyboardAct state (Char '\r') Down = actionEvalInputLine state
keyboardAct state (Char c) Down = actionModifyInputLine (IL.input c) state
keyboardAct state (SpecialKey KeyLeft) Down = actionModifyInputLine IL.left state
keyboardAct state (SpecialKey KeyRight) Down = actionModifyInputLine IL.right state
keyboardAct state (SpecialKey KeyHome) Down = actionModifyInputLine IL.pos1 state
keyboardAct state (SpecialKey KeyEnd) Down = actionModifyInputLine IL.end state

-- Function key bindings
keyboardAct state (SpecialKey KeyF1) Down = actionToggle showHelp state
keyboardAct state (SpecialKey KeyF2) Down = actionToggle showREPL state
keyboardAct state (SpecialKey KeyF3) Down = actionToggle showFramesPerSecond state
keyboardAct state (SpecialKey KeyF5) Down = actionReloadSource state
keyboardAct state (SpecialKey KeyF6) Down = actionResetView state
keyboardAct state (SpecialKey KeyF7) Down = actionScreenshot state
keyboardAct _ _ _ = return ()

keyboardMouseHandler state key st modifiers position = do
  lastPosition state $= (Position (-1) (-1))
  keyboardAct state key st

mouseHandler state keystate mod pos@(Position x y) = do
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

