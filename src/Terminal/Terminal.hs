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

{-
 - Todo:
 - local echo
 - when at bottom, move screen up and add empty line
 - 1[5];24r
 - ANSI as parsec parser and as own file (VT100.hs?)
 -}

line :: Parser [Int]
line = number `sepBy` (char ';' *> spaces)

number = read <$> many digit

-- | Standard build function.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

chunk :: Int -> [s] -> [[s]]
chunk i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

data Terminal = Terminal {
    cursorPos :: (Int, Int),
    screen :: UArray (Int, Int) Char,
    state :: TerminalState,
    ansiCommand :: String,
    inBuffer :: String,
    rows :: Int,
    cols :: Int
}

data TerminalState =
    WaitingForInput |
    ANSIEscape |
    ReadingANSI

emptyChar = ' '

initTerm s@(rows, cols) = Terminal {
    state = WaitingForInput,
    cursorPos = (1, 1),
    ansiCommand = "",
    rows = rows,
    cols = cols,
    inBuffer = "",
    screen = array
        ((1, 1), s)
        [((y, x), emptyChar) | x <- [1..cols], y <- [1..rows]]
}

up t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y - 1, x) }
down t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y + 1, x) }
left t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x - 1) }
right t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x + 1) }


-- Wrap line
safeCursor t@Terminal {cursorPos = (y, 81) } =
    safeCursor $ t { cursorPos = (y + 1, 1) }

safeCursor term@Terminal {cursorPos = (y, x), cols = c, rows = r } =
    term { cursorPos = (min r (max 1 y), min c (max 1 x)) }

handleANSI t | trace ("ANSI " ++ show (ansiCommand t)) False = undefined
handleANSI term@Terminal {ansiCommand = c, cursorPos = (y, x), screen = s }
    | c == "A"    = up term
    | c == "B"    = down term
    | c == "C"    = right term
    | c == "D"    = left term
    | c == "H"    = term { cursorPos = (1, 1) }

    -- Erases the screen from the current line down to the bottom of the screen.
    | c == "J"    = term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[y..24]] }
    | c == "0J"    = term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[y..24]] }

    -- Erases the screen from the current line up to the top of the screen.
    | c == "1J"    = term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[1..y]] }

    -- Erases the screen with the background color and moves the cursor to home.
    | c == "2J"    = term { cursorPos = (1, 1), screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[1..24]] }

    | c == "K"    = term { screen = s // [((y,x_), emptyChar)|x_<-[1..80]] }
    | c == ""   = term

    -- Enable scrolling (for whole sceen)
    | c == "r"   = term

    -- Device attributes
    | last c == 'c'   = term { inBuffer = "\ESC[1;0c" }
    | c !! 0 == '?'   = term { inBuffer = "\ESC[1;0c" }

    -- Set line attributes
    | last c == 'l'   = term
    | last c == 'h'   = term

    -- Set colors
    | last c == 'm'   = term

    | last c =='A'= case parse line "" c of
        Left error -> term
        Right [times] -> (iterate down term) !! times
    | last c =='B'= case parse line "" c of
        Left error -> term
        Right [times] -> (iterate up term) !! times
    | last c =='C'= case parse line "" c of
        Left error -> term
        Right [times] -> (iterate right term) !! times
    | last c =='D'= case parse line "" c of
        Left error -> term
        Right [times] -> (iterate left term) !! times
    | last c =='H'= case parse line "" c of
        Left error -> term
        Right [y_,x_] -> safeCursor $ term { cursorPos = (y_, x_) }
    | otherwise   = trace ("UNKNOWN ANSI " ++ show (ansiCommand term)) undefined

handleChar :: Char -> Terminal -> Terminal
handleChar '\ESC' term =
    term { state = ANSIEscape }
handleChar '[' term@Terminal { state = ANSIEscape } =
    term { state = ReadingANSI, ansiCommand = "" }

handleChar c term@Terminal { state = ReadingANSI }
    | isNumber c || elem c ";=?" = term { ansiCommand = (ansiCommand term) ++ [c] }
    | otherwise                  = handleANSI $ term { ansiCommand = (ansiCommand term) ++ [c], state = WaitingForInput }

handleChar '\a' t = t -- BELL

handleChar '\r' term@Terminal { state = WaitingForInput } =
    let pos@(y, x) = cursorPos term in
    term { cursorPos = (y, 1) }
handleChar '\b' term@Terminal { state = WaitingForInput, screen = s } =
    let pos@(y, x) = cursorPos term in
    safeCursor $ term { cursorPos = (y, x - 1), screen = s // [(pos, emptyChar)] }
handleChar '\n' term@Terminal { state = WaitingForInput } =
    let pos@(y, x) = cursorPos term in
    safeCursor $ term { cursorPos = (y + 1, 1) }
handleChar c term@Terminal { state = WaitingForInput, screen = s } =
    let pos@(y, x) = cursorPos term in
    safeCursor $ term { screen = s // [(pos, c)], cursorPos = (y, x + 1) }

handleChar c t = t

wrap d s = d ++ s ++ d

printTerm term = do
    putStr "\ESC[2J"
    print $ (cursorPos term, ansiCommand term)
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
        liftIO $ print c -- \        liftIO $ putStrLn "----"
        modify (handleChar c)
        s <- get
        when ((inBuffer s) /= "") $ do
            liftIO $ putStrLn "writing sth"
            liftIO $ hPutStr in_ (inBuffer s)
            modify $ \t -> t { inBuffer = "" }
        isReady <- liftIO $ hReady out
        when (not isReady) $ do
            liftIO $ printTerm s

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
