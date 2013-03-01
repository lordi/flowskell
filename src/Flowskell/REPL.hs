-- module Flowskell.REPL where
import Control.Monad.State

data InputLine = InputLine { buffer :: String, cursorPosition :: Int } -- deriving Show

instance Show InputLine where
    show InputLine { buffer = b, cursorPosition = c } =
            let (b1,b2) = splitAt c b in show $ b1 ++ "%" ++ b2

inputStr str = mapM_ input str

input :: Char -> State InputLine ()
input chr = modify $ \InputLine { buffer = b, cursorPosition = c } ->
            let (b1,b2) = splitAt c b in
            InputLine { buffer = b1 ++ [chr] ++ b2, cursorPosition = c + 1 }

backspace :: State InputLine ()
backspace = modify $ \il@InputLine { buffer = b, cursorPosition = c } ->
            let (b1,b2) = splitAt c b in
            il { buffer = (take (c - 1) b1) ++ b2, cursorPosition = max (c - 1) 0 }

pos1 :: State InputLine ()
pos1 = modify $ \il -> il { cursorPosition = 0 }

end :: State InputLine ()
end = modify $ \il@InputLine { buffer = b } -> il { cursorPosition = length b }

del :: State InputLine ()
del = modify $ \il@InputLine { buffer = b, cursorPosition = c } ->
            let (b1,b2) = splitAt c b in il { buffer = b1 ++ (tail b2) }

testInput :: State InputLine ()
testInput = do
    input 'k'
    input 'e'
    input 'l'
    input 'l'
    input 'o'
    input 'i'
    input 'p'
    backspace
    backspace
    input ' '
    inputStr "world"
    pos1
    backspace
    backspace
    backspace
    del
    input 'h'
    end
    end
    input '!'

repl :: State InputLine ()
repl = do
    input 'c'

newInputLine = InputLine { buffer = "ha", cursorPosition = 0 }
main = do
--    print $ runState testInput $ InputLine { buffer = "", cursorPosition = 0 }
    let il = newInputLine
    print $ runState InputLine repl $ il

    -- print s
