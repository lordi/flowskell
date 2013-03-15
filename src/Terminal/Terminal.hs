{-# LANGUAGE Rank2Types #-}
module Terminal (newTerminal, applyAction) where
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

newTerminal s@(rows, cols) = Terminal {
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

applyAction :: TerminalAction -> Terminal -> Terminal
applyAction act term@Terminal { screen = s, cursorPos = pos@(y, x) } =
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

