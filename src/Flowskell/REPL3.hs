import System.IO
import Control.Monad.State
import Data.Char

type InputLine = (String, String) -- String before cursor, after cursor

initialInputLine = ("", "")
drop1FromEnd s = take (length s - 1) s
last1 s = drop (length s - 1) s

input c (b, a) = (b ++ [c], a)
backspace (b, a) = (drop1FromEnd b, a)
pos1 (b, a) = ("", b ++ a)
end (b, a) = (b ++ a, "")
del (b, a) = (b, drop 1 a)
left (b, a) = (drop1FromEnd b, (last1 b) ++ a)
right (b, a) = (b ++ [a], drop 1 a)

handleChar '\x08' = backspace
handleChar '\x7e' = del
handleChar '(' = (input '(') . left . (input ')')
handleChar c = input c
handleEscapeSequence 'H' = pos1
handleEscapeSequence 'F' = end
handleEscapeSequence 'D' = left
handleEscapeSequence 'C' = right
handleEscapeSequence _ = id

repl :: StateT InputLine IO ()
repl = do
    forever $ do
        c <- liftIO $ getChar
        if (c == '\x1b')
            then do
                '[' <- liftIO $ getChar
                c2 <- liftIO $ getChar
                modify (handleEscapeSequence c2)
            else modify (handleChar c)
        liftIO $ putStrLn ""
        get >>= liftIO . print

main = do
    hSetBuffering stdin NoBuffering
    runStateT repl initialInputLine

