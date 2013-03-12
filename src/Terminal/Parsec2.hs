import Text.Parsec

play :: String -> Either ParseError (Integer,Int)
play s = runParser pmain 0 "parameter" s  -- 0 is the initial value

-- type Parsec s u a = ParsecT s u Identity a
pmain :: Parsec [Char] Int (Integer,Int)
pmain = do
  x <- pnum `chainl1` pplus
  eof
  n <- getState  -- obtain the user state value
  return (x,n)

pnum = do
  x <- read `fmap` many1 digit
  modifyState (1 +)  -- change the user state value
  return x

pplus = char '+' >> return (+)
