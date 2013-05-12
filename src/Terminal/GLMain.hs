{-# LANGUAGE Rank2Types #-}
import System.Process
import Data.Array.Unboxed
import Data.IORef
import Data.Char
import Control.Monad
import Control.Monad.State hiding (state)
import System.IO

import Graphics.UI.GLUT hiding (Bool, Float, get)
import Graphics.Rendering.GLU.Raw
import Graphics.Rendering.OpenGL hiding (Bool, Float, get)
import Graphics.Rendering.OpenGL.GLU (perspective)
import Graphics.Rendering.OpenGL.GL.FramebufferObjects
import Graphics.Rendering.OpenGL.GL.Texturing.Environments
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (glPushMatrix, glPopMatrix) -- TODO check if these are really needed
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import GHC.IO.Handle
import Debug.Trace
import Control.Concurrent
import Control.Applicative hiding (many)
import Text.Parsec
import Text.Parsec.String

import Parser
import Terminal
import Types


initDisplay = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer]
  createWindow "Termskell"

  materialShininess Front $= 0.0
  shadeModel $= Smooth
  frontFace $= CW
  autoNormal $= Enabled
  normalize $= Enabled
  depthFunc $= Just Less
  -- cullFace $= Just Back

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  clearColor $= Color4 0 0 0 1


displayHandler a = do
  clear [ColorBuffer, DepthBuffer]

  term <- readIORef a
  -- printTerm term

  -- color $= Color4 1 1 1 1

  let withTextMode sth = do
      textureBinding Texture2D $= Nothing
      matrixMode $= Modelview 0
      preservingMatrix $ do
          loadIdentity
          sth

  withTextMode $ do
    currentRasterPosition $= Vertex4 (-1) (0.95) 0 (1::GLfloat)
    renderString Fixed9By15 $ show ((cursorPos term), inBuffer term)
    let lines = chunk (cols term) $ elems (screen term)
        (y, x) = cursorPos term
    forM_ (zip [1..] lines) $ \(i, s) -> do
        currentRasterPosition $= Vertex4 (-1) (0.9 - i / 14.0) 0 (1::GLfloat)
        renderString Fixed9By15 s

    -- Show cursor
    currentRasterPosition $= Vertex4 (-1 + fromIntegral x / 40.0) (0.9 - fromIntegral y / 14.0) 0 (1::GLfloat)
    renderString Fixed9By15 "%"

  swapBuffers

{-
 - Todo:
 [ ] local echo
 [ ] when at bottom, move screen up and add empty line
 [ ] 1[5];24r
 [ ] ANSI as parsec parser and as own file (VT100.hs?)
 -}

-- | Standard build function.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

chunk :: Int -> [s] -> [[s]]
chunk i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

wrap d s = d ++ s ++ d

printTerm term = do
--    putStr "\ESC[2J"
    print $ (cursorPos term)
    ---, ansiCommand term)
--    when ( ((ansiCommand term) /= "") && last (ansiCommand term) == 'H') $ print $ "=========>" ++ (ansiCommand term)
    putStrLn $ "," ++ (replicate (cols term) '_') ++ ","
    mapM_
        (putStrLn . (wrap "|"))
        (chunk (cols term) $ elems (screen term // [(cursorPos term, '|')]))
    putStrLn $ "`" ++ (replicate (cols term) '"') ++ "´"
    hFlush stdout

runTerminal :: IORef Terminal -> Handle -> Handle -> IO ()
runTerminal a in_ out = do
    forever $ do
        c <- (liftIO $ hGetChar out)
        s <- readIORef a

        Right (actions, leftover) <- return $ play $ (inBuffer s) ++ [c]
        -- liftIO $ print c
        -- liftIO $ print actions
        -- liftIO $ print leftover

        forM actions (\x -> do
--            liftIO $ putStrLn $ "executing" ++ show x
            modifyIORef a $ applyAction x)

        term <- readIORef a
        writeIORef a $ term { inBuffer = leftover }
        
{-        s <- get

        when ((inBuffer s) /= "") $ do
            liftIO $ putStrLn "writing sth"
            liftIO $ hPutStr in_ (inBuffer s)
            modify $ \t -> t { inBuffer = "" } -}
--        isReady <- liftIO $ hReady out
        --unless isReady $ do
        --    print "redisplay..."
--        postRedisplay Nothing
--        postOverlayRedisplay Nothing
--            liftIO $ printTerm term

redirect :: Handle -> Handle -> IO ()
redirect from to =
    forever $ do
        hGetChar from >>= hPutChar to

keyboardMouseHandler hInWrite (Char c) Down modifiers position = do
    hPutChar hInWrite c
keyboardMouseHandler hInWrite chr st modifiers position = do return ()

main = do
    (pOutRead, pOutWrite) <- createPipe
    (pInRead, pInWrite) <- createPipe
    (pErrRead, pErrWrite) <- createPipe

    hInRead <- fdToHandle pInRead
    hInWrite <- fdToHandle pInWrite
    hOutRead <- fdToHandle pOutRead
    hOutWrite <- fdToHandle pOutWrite

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    hSetBuffering hInRead NoBuffering
    hSetBuffering hInWrite NoBuffering
    hSetBuffering hOutRead NoBuffering
    hSetBuffering hOutWrite NoBuffering

    initDisplay

    a <- newIORef defaultTerm

    displayCallback $= displayHandler a
    idleCallback $= Just (postRedisplay Nothing)
    keyboardMouseCallback $= Just (keyboardMouseHandler hInWrite)

    let environment = [
            ("TERM", "vt100"),
            ("COLUMS", "79"),
            ("ROWS", "24")]
    process <- runProcess "script" ["-c", "bash --init-file .bashrc", "-f", "/dev/null"] Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    -- 
    -- forkIO $ redirect stdin hInWrite
    forkIO $ runTerminal a hInWrite hOutRead
    -- forkIO $ runStateT (runTerminal hInWrite hOutRead) (initTerm (24, 80)) >> return ()
    mainLoop
