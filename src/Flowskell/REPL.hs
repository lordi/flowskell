-- module Flowskell.REPL where
import Control.Monad.State

data InputState = InputState { buffer :: String, cursorPosition :: Int } deriving Show

input :: Char -> State InputState () --IO ()
input chr = do
    InputState { buffer = b, cursorPosition = c } <- get
    put InputState { buffer = b ++ [chr], cursorPosition = c + 1 }

backspace :: State InputState () --IO ()
backspace = do
    InputState { buffer = b, cursorPosition = c } <- get
    put InputState { buffer = b, cursorPosition = c - 1 }

testInput :: State InputState ()
testInput = do
    input 'a'
    input 'a'
    input 'c'
    input 'a'
    backspace

main = do
    ((), s) <- runState testInput $ InputState { buffer = "", cursorPosition = 0 }
    print s
