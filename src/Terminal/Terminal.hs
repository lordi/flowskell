{-# LANGUAGE Rank2Types #-}
module Terminal where
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

import Parser
import Types

emptyChar = ' '

initTerm s@(rows, cols) = Terminal {
    cursorPos = (1, 1),
    rows = rows,
    cols = cols,
    inBuffer = "",
    responseBuffer = "",
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

{-
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
-}


handleAction :: TerminalAction -> Terminal -> Terminal
handleAction act term@Terminal { screen = s, cursorPos = pos@(y, x) } =
    safeCursor t
    where t = case act of
            Ignored             -> term

            -- Bell
            CharInput '\a'      -> term

            -- Newline
            CharInput '\n'      -> term { cursorPos = (y + 1, 1) }
            CharInput '\r'      -> term { cursorPos = (y, 1) }
            CharInput '\b'      -> term { screen = s // [(pos, emptyChar)], cursorPos = (y, x - 1) }
            CharInput c         -> term { screen = s // [(pos, c)], cursorPos = (y, x + 1) }

            CursorUp n          -> (iterate up term) !! n
            CursorDown n        -> (iterate down term) !! n
            CursorForward n     -> (iterate right term) !! n
            CursorBackward n    -> (iterate left term) !! n

            -- Colors, yay!
            ANSIAction _ 'm'    -> term

            -- Force cursor position
            SetCursor col row   -> term { cursorPos = (col, row) }

            -- Erases the screen with the background color and moves the cursor to home.
            ANSIAction [2] 'J'  -> term { cursorPos = (1, 1), screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[1..24]] }

            -- Erases the screen from the current line up to the top of the screen.
            ANSIAction [1] 'J'  -> term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[1..y]] }

            -- Erases the screen from the current line down to the bottom of the screen.
            ANSIAction _ 'J'    -> term { screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-[y..24]] }

            -- Erases the entire current line.
            ANSIAction [2] 'K'  -> term { screen = s // [((y,x_), emptyChar)|x_<-[1..80]] }

            -- Erases from the current cursor position to the start of the current line.
            ANSIAction [1] 'K'  -> term { screen = s // [((y,x_), emptyChar)|x_<-[1..x]] }

            -- Erases from the current cursor position to the end of the current line. 
            ANSIAction _ 'K'  -> term { screen = s // [((y,x_), emptyChar)|x_<-[x..80]] }

            _                 -> trace ("\nunsupported seq: " ++ show act) term

