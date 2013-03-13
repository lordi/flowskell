import Text.Parsec
import Text.Parsec.String

ps :: Parser String
ps = (string "123") <|> try (string "456") <|> fail "my-failure"

main = do
    putStrLn $ "A: " ++ show (parse ps "" "123")
    putStrLn $ "\nB: " ++ show (parse ps "" "789")
    putStrLn $ "\nC: " ++ show (parse ps "" "45x")
