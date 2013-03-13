import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Control.Monad.State
import System.IO
import System.Exit
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Debug.Trace
import Data.List (insert)
import Data.Maybe (maybeToList)
import qualified Text.Parsec.Token as PT

data TerminalAction c =
    Bell
    | Newline
    | TChar c
    | ANSIAction [Int] Char
    | Ignored
    deriving Show

play :: String -> Either ParseError ([TerminalAction Char], String)
play s = parse pxxx "" s

pnum = read `fmap` many1 digit

pxxx :: Parser ([TerminalAction Char], String)
pxxx = do
    x <- many (pchar <|> try pansi <|> try pansi2 <|> try pansi3)
    i <- getInput
    return (x, i)

pansi :: Parser (TerminalAction Char)
pansi = do
    string "\ESC["
    optionMaybe (char '?')
    param <- optionMaybe pnum
    params <- many (char ';' >> pnum)
    c <- letter
    return $ ANSIAction (maybeToList param ++ params) c

pansi2 :: Parser (TerminalAction Char)
pansi2 = do
    string "\ESC="
    return Ignored

pansi3 :: Parser (TerminalAction Char)
pansi3 = do
    string "\ESC("
    return Ignored

pchar :: Parser (TerminalAction Char)
pchar = (newline >> return Newline)
    <|> (char '\a' >> return Bell)
    <|> (satisfy (/= '\ESC') >>= return . TChar)

stdinReader =
    forever $ do
        (liftIO getChar) >>= \x -> modify $ \y -> y ++ [x]
        s <- get
        case (play s) of
            Right (p, leftover) -> (liftIO $ print (p, leftover)) >> put leftover
            Left b -> (liftIO $ print (b)) -- >> (liftIO $ exitFailure)
        return ()

main = do
    hSetBuffering stdin NoBuffering
    print $ play "wldjawlkdj1234\a\n\n\ESC[0m\ESC[1;6m\ESC[2K\ESC[A\n12\n"
    runStateT stdinReader ""
