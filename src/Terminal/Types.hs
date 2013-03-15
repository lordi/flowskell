{-# LANGUAGE Rank2Types #-}
module Types where
import System.Process
import Data.Array.Unboxed
import Data.Char

data TerminalAction =
      CharInput Char
    | CursorUp Int
    | CursorDown Int
    | CursorForward Int
    | CursorBackward Int
    | SetCursor Int Int
    | ANSIAction [Int] Char
    | KeypadKeysApplicationsMode
    | KeypadKeysNumericMode
    | Ignored
    deriving Show

data Terminal = Terminal {
    cursorPos :: (Int, Int),
    screen :: UArray (Int, Int) Char,
    inBuffer :: String,
    responseBuffer :: String,
    rows :: Int,
    cols :: Int
}

