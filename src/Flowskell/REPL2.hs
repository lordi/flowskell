import System.IO
import Control.Monad.State
import Data.Char

-- data InputLine = InputLine { before :: String, after :: String } deriving Show 
-- instance Show InputLine where
--    show l = (before l) ++ "%" ++ (after l)

type InputLine = (String, String)
-- InputLine { before :: String, after :: String } deriving Show 
-- input c l = apply (++ [c], id)


input c l = l { before = before l ++ [c] }
initialInputLine = InputLine { before = "", after = "" }
drop1FromEnd s = take (length s - 1) s
last1 s = drop (length s - 1) s
backspace l = l { before = drop1FromEnd (before l) }
pos1 l = l { before = "", after = before l ++ after l }
end l = l { before = before l ++ after l, after = "" }
del l = l { after = drop 1 (after l) }
left l = l { before = drop1FromEnd (before l), after = (last1 (before l)) ++ (after l) }
right l = l { before = (before l) ++ (take 1 (after l)), after = drop 1 (after l) }

handleChar '\x08' = backspace
handleChar '\x7e' = del
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

