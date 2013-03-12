import Text.Parsec
import Text.Parsec.String

play :: String -> Either ParseError Integer
play s = parse pmain "parameter" s

pmain :: Parser Integer
pmain = do
  x <- pnum `chainl1` pplus
  eof
  return x

pnum = read `fmap` many1 digit

pplus = char '+' >> return (+)
