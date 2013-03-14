{-# LANGUAGE Rank2Types #-}
import System.Process
import Data.Array.Unboxed
import Data.Char
import Control.Monad
import Control.Monad.State hiding (state)
import System.IO
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

runTerminal :: Handle -> Handle -> StateT Terminal IO ()
runTerminal in_ out = do
    forever $ do
        c <- (liftIO $ hGetChar out)
        s <- get

        Right (actions, leftover) <- return $ play $ (inBuffer s) ++ [c]
        liftIO $ print c
        liftIO $ print actions
        liftIO $ print leftover

        forM actions (\x -> do
            liftIO $ putStrLn $ "executing" ++ show x
            modify $ handleAction x)

        term <- get
        put $ term { inBuffer = leftover }
        
{-        s <- get

        when ((inBuffer s) /= "") $ do
            liftIO $ putStrLn "writing sth"
            liftIO $ hPutStr in_ (inBuffer s)
            modify $ \t -> t { inBuffer = "" } -}
        isReady <- liftIO $ hReady out
        when (not isReady) $ do
            liftIO $ printTerm term

redirect :: Handle -> Handle -> IO ()
redirect from to =
    forever $ do
        hGetChar from >>= hPutChar to

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

    let environment = [
            ("TERM", "vt100"),
            ("COLUMS", "79"),
            ("ROWS", "10")]
    process <- runProcess "script" ["-c", "bash", "-f", "/dev/null"] Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    forkIO $ redirect stdin hInWrite
    runStateT (runTerminal hInWrite hOutRead) (initTerm (24, 80))
