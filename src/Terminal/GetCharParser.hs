import Control.Monad
import Control.Monad.State
import System.IO
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Data.List (insert)

data TerminalAction c = Bell | Newline | TChar c | ANSIAction deriving Show

play :: String -> Either ParseError [TerminalAction Char]
play s = parse pxxx "" s

pxxx :: Parser [TerminalAction Char]
pxxx = many (pchar <|> pansi)

pansi = do
    string "\ESC]"
    many digit
    letter
    return ANSIAction
pchar = (newline >> return Newline)
    <|> (char '\a' >> return Bell)
    <|> (satisfy (/= '\ESC') >>= return . TChar)

stdinReader =
    forever $ do
        (liftIO getChar) >>= \x -> modify $ \y -> y ++ [x]
        s <- get
        liftIO $ print (s, play s)

main = do
    hSetBuffering stdin NoBuffering
    print $ play "awldjawlkdj1234\a\n\n\ESC]m\ESC]2K\n12\n"
    runStateT stdinReader ""
