import Control.Monad
import Control.Monad.State
import System.IO
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Data.List (insert)

data TerminalAction c = Newline | TChar c | ANSIAction deriving Show

playxxx :: String -> Either ParseError [TerminalAction Char]
playxxx s = parse pxxx "parameter" s

pxxx :: Parser [TerminalAction Char]
pxxx = many (pchar <|> pansi)
pansi = do
    char 'E'
    char ']'
    return ANSIAction
pchar = (char '\n' >> return Newline) <|> (digit >>= return . TChar)

stdinReader =
    forever $ do
        (liftIO getChar) >>= \x -> modify $ \y -> y ++ [x]
        s <- get
        liftIO $ print (s, playxxx s)

main = do
    hSetBuffering stdin NoBuffering
    runStateT stdinReader ""
