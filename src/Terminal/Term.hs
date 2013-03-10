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
    inBuffer :: String
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
    inBuffer = "",
    screen = array
        ((1, 1), s)
        [((y, x), emptyChar) | x <- [1..cols], y <- [1..rows]]
}

up term@Terminal {cursorPos = (y, x)} = term { cursorPos = (max (y - 1) 1, x) }
down term@Terminal {cursorPos = (y, x)} = term { cursorPos = (min (y + 1) 25, x) }
left term@Terminal {cursorPos = (y, x)} = term { cursorPos = (y, max (x - 1) 1) }
right term@Terminal {cursorPos = (y, x)} = term { cursorPos = (y, min (x + 1) 80) }

safeCursor term@Terminal {cursorPos = (y, x) } =
    term { cursorPos = (min 25 (max 1 y), min 80 (max 1 x)) }

handleANSI t | trace ("ANSI " ++ show (ansiCommand t)) False = undefined
handleANSI term@Terminal {ansiCommand = c, cursorPos = (y, x), screen = s }
    | c == "K"    = term
    | c == "A"    = up term
    | c == "B"    = down term
    | c == "C"    = right term
    | c == "D"    = left term
    | c == "H"    = term { cursorPos = (1, 1) }

    -- Erases the screen from the current line down to the bottom of the screen.
    | c == "J"    = term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[y..25]] }
    | c == "0J"    = term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[y..25]] }

    -- Erases the screen from the current line up to the top of the screen.
    | c == "1J"    = term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[1..y]] }

    -- Erases the screen with the background color and moves the cursor to home.
    | c == "2J"    = term { cursorPos = (1, 1), screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[1..25]] }

    | c == "K"    = term { screen = s // [((y,x_), emptyChar)|x_<-[x..80]] }
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


handleChar '\r' term@Terminal { state = WaitingForInput } =
    let pos@(y, x) = cursorPos term in
    term { cursorPos = (y, 1) }
handleChar '\b' term@Terminal { state = WaitingForInput } =
    let pos@(y, x) = cursorPos term in
    term { cursorPos = (y, max 1 (x - 1)) }
--           screen = (screen term) // [(pos, '_')] }
handleChar '\n' term@Terminal { state = WaitingForInput } =
    let pos@(y, x) = cursorPos term in
    term { cursorPos = (y + 1, 1) }
handleChar '\a' t = t -- BELL
handleChar c term@Terminal { state = WaitingForInput } =
    let pos@(y, x) = cursorPos term in
    term { screen = (screen term) // [(pos, c)],
           cursorPos = (y, min 80 (x + 1)) }

handleChar c t = t

wrap d s = d ++ s ++ d

printTerm term = do
    putStr "\ESC[2J"
    print $ (cursorPos term, ansiCommand term)
--    when ( ((ansiCommand term) /= "") && last (ansiCommand term) == 'H') $ print $ "=========>" ++ (ansiCommand term)
    putStrLn $ "," ++ (replicate 80 '_') ++ ","
    mapM_ (putStrLn . (wrap "|")) (chunk 80 $ elems (screen term // [(cursorPos term, '|')]))
    putStrLn $ "`" ++ (replicate 80 '"') ++ "´"
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
        liftIO $ printTerm s

redirect :: Handle -> Handle -> IO ()
redirect from to =
    forever $ do
        print "read"
        hGetChar from >>= hPutChar to
        print "wrote"
        hFlush to

main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    -- (_in, _out, _err, proccess) <- runInteractiveCommand "ls -al --color"
    --(_in, _out, _err, proccess) <- runInteractiveCommand "vi"
    (pOutRead, pOutWrite) <- createPipe
    (pInRead, pInWrite) <- createPipe
    _err <- createPipe

    (termMaster, termSlave) <- openPseudoTerminal

    hTermSlave <- fdToHandle termSlave
    hInRead <- fdToHandle pInRead
    hInWrite <- fdToHandle pInWrite

    hSetBuffering hInRead NoBuffering
    hSetBuffering hInWrite NoBuffering

    hOutRead <- fdToHandle pOutRead
    hOutWrite <- fdToHandle pOutWrite
    hSetBuffering hOutRead NoBuffering
    hSetBuffering hOutWrite NoBuffering

    let environment = [
            ("TERM", "vt100"),
            ("COLUMS", "79"),
            ("ROWS", "10")]
    process <- runProcess "top" [] Nothing (Just environment)
    -- process <- runProcess "vttest" ["24x80.80"] Nothing (Just environment)
    -- process <- runProcess "vi" [] Nothing (Just environment)
            (Just hInRead) (Just hOutWrite) Nothing
    -- ttest"
    -- hSetBuffering _in NoBuffering


    forkIO $ redirect stdin hInWrite
    print $ parse line "" "25;8H"
    runStateT (runTerminal hInWrite hOutRead) (initTerm (25, 80))
