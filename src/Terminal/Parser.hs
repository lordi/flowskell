module Parser where
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

import Types

-- TODO: choose another name
simplify :: TerminalAction -> TerminalAction
simplify (ANSIAction [] 'A') = CursorUp 1
simplify (ANSIAction [n] 'A') = CursorUp n
simplify (ANSIAction [] 'B')  = CursorDown 1
simplify (ANSIAction [n] 'B')  = CursorDown n
simplify (ANSIAction [] 'C') = CursorForward 1
simplify (ANSIAction [n] 'C') = CursorForward n
simplify (ANSIAction [] 'D') = CursorBackward 1
simplify (ANSIAction [n] 'D') = CursorBackward n
simplify (ANSIAction [start, end] 'r') = SetScrollingRegion start end

simplify (ANSIAction [] 'H') = SetCursor 1 1
simplify (ANSIAction [] 'f') = SetCursor 1 1
simplify (ANSIAction [y,x] 'H') = SetCursor y x
simplify (ANSIAction [y,x] 'f') = SetCursor y x
simplify x = x

play :: String -> Either ParseError ([TerminalAction], String)
play s = parse pxxx "" s

pnum = read `fmap` many1 digit

pxxx :: Parser ([TerminalAction], String)
pxxx = do
    x <- many (pchar <|> pansi)
    i <- getInput
    return (map simplify x, i)

pansi :: Parser (TerminalAction)
pansi = try (do
    string "\ESC["
    optionMaybe (char '?')
    param <- optionMaybe pnum
    params <- many (char ';' >> pnum)
    c <- letter
    return $ ANSIAction (maybeToList param ++ params) c
    )
    <|> try (string "\ESCM" >> return ScrollUp)
    <|> try (string "\ESCD" >> return ScrollDown)
    <|> try (string "\ESC=" >> return KeypadKeysApplicationsMode)
    <|> try (string "\ESC>" >> return KeypadKeysNumericMode)
    <|> try (string "\ESC(B" >> return Ignored)
    <|> try (string "\ESC#8" >> return Ignored)

pchar :: Parser (TerminalAction)
pchar = (satisfy (/= '\ESC') >>= return . CharInput)

{-
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
    -}
