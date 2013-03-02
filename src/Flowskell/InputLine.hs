-- Lightweight and reusable Haskeline alternative
module Flowskell.InputLine (
    InputLine,
    newInputLine,
    getInput,
    input, backspace, pos1, end, del, left, right
    ) where
import System.IO
import Control.Monad.State
import Data.Char

type InputLine = (String    -- String before cursor
                , String)   -- String after cursor
type InputLineModifier = InputLine -> InputLine

newInputLine :: InputLine
newInputLine = ("", "")

getInput :: InputLine -> String
getInput (b, a) = b ++ a

input :: Char -> InputLineModifier
input c (b, a) = (b ++ [c], a)

backspace :: InputLineModifier
backspace (b, a) = (drop1FromEnd b, a)

pos1 :: InputLineModifier
pos1 (b, a) = ("", b ++ a)

end :: InputLineModifier
end (b, a) = (b ++ a, "")

del :: InputLineModifier
del (b, a) = (b, drop 1 a)

left :: InputLineModifier
left (b, a) = (drop1FromEnd b, (last1 b) ++ a)

right :: InputLineModifier
right (b, a) = (b ++ (take 1 a), drop 1 a)

drop1FromEnd s = take (length s - 1) s
last1 s = drop (length s - 1) s

-- The following functionality is only for testing/evaluating.
ansiInput :: String -> StateT InputLine IO ()
ansiInput prompt = do
    liftIO $ putStr prompt
    forever $ do
        c <- liftIO $ getChar
        if (c == ansiEsc)
            then do
                escCode <- liftIO $ getChar >> getChar
                modify (handleEscapeSequence escCode)
            else modify (handleChar c)
        (b, a) <- get
        liftIO $ putStr $ "\r" ++ prompt ++ b ++ a ++ "\x1b[K\r" ++ prompt ++ b
    where
        ansiEsc = '\x1b'
        handleChar '\x08' = backspace
        handleChar '\x7e' = del
        handleChar c = input c
        handleEscapeSequence 'H' = pos1
        handleEscapeSequence 'F' = end
        handleEscapeSequence 'D' = left
        handleEscapeSequence 'C' = right
        handleEscapeSequence _ = id

main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    runStateT (ansiInput ">>> ") newInputLine
