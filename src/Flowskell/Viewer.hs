module Flowskell.Viewer where
import Control.Monad (when)
import Graphics.UI.GLUT hiding (Bool, Float)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (getModificationTime, doesFileExist)

import Flowskell.State
import Flowskell.Interpreter (initSchemeEnv, initPrimitives)
import Flowskell.InputActions (
    actionReloadSource, motionHandler, mouseHandler,
    keyboardMouseHandler)
import Flowskell.Display (
    initDisplay, reshapeHandler, displayHandler)

viewer = do
  (_, [filename]) <- getArgsAndInitialize
  state <- makeState filename
  initDisplay state
  extraPrimitives <- initPrimitives state
  let initFunc' = initSchemeEnv extraPrimitives
  initFunc state $= Just initFunc'
  env <- initFunc' filename
  environment state $= Just env
  idleCallback $= Just (idle state)

  -- Flowskell.Display
  displayCallback $= displayHandler state
  reshapeCallback $= Just (reshapeHandler state)

  -- Flowskell.InputActions
  motionCallback $= Just (motionHandler state)
  mouseCallback $= Just (mouseHandler state)
  keyboardMouseCallback $= Just (keyboardMouseHandler state)

  -- GLUT main loop
  mainLoop


-- |Idle function. Check modifcation date of the current source file
--  and reload the environment when it has changed.
idle state = do
  now <- getCurrentTime

  lastCheckTime <- get $ lastReloadCheck state
  when (diffUTCTime now lastCheckTime > 0.5) $ do
    Just env <- get $ environment state
    doesFileExist (source state) >>= \x -> when x $ do
      modTime <- getModificationTime (source state)
      lastModTime <- get $ lastSourceModification state
      when (lastModTime < modTime) $ actionReloadSource state
      lastSourceModification state $= modTime
      lastReloadCheck state $= now

  lastFPSTime <- get $ lastFrameCounterTime state
  when (diffUTCTime now lastFPSTime > 0.75) $ do
    cnt <- get $ frameCounter state
    framesPerSecond state $= (fromIntegral cnt / realToFrac (diffUTCTime now lastFPSTime))
    frameCounter state $= 0
    lastFrameCounterTime state $= now

  postRedisplay Nothing

