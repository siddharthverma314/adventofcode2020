import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

data Rule = Rule {letter :: Char, from :: Int, to :: Int} deriving (Show)

-- parsing stuff
parseInt :: ReadP Int
parseInt = do
  n <- many1 $ satisfy isDigit
  return $ read n

parseChar :: ReadP Char
parseChar = satisfy isAlpha

parseRule :: ReadP Rule
parseRule = do
  from <- parseInt
  char '-'
  to <- parseInt
  char ' '
  letter <- parseChar
  return $ Rule letter from to

parseRuleAndPassword :: ReadP (Rule, String)
parseRuleAndPassword = do
  rule <- parseRule
  string ": "
  password <- munch isAlpha
  return (rule, password)

-- verify password
safeIndex :: [a] -> Int -> Maybe a
safeIndex lst x
  | x <= 0 || x > length lst = Nothing
  | otherwise = Just $ lst !! (x - 1)

verifyPassword :: Rule -> String -> Bool
verifyPassword rule password = from rule <= count && count <= to rule
  where
    count :: Int
    count = length $ filter ((==) $ letter rule) password

verifyPassword2 :: Rule -> String -> Bool
verifyPassword2 rule password = count == 1
  where
    chars :: [Maybe Char]
    chars = [password `safeIndex` from rule, password `safeIndex` to rule]
    count :: Int
    count = length $ filter (\c -> Just (letter rule) == c) chars

main :: IO ()
main = do
  text <- readFile "day2_1.txt"
  let rulesAndPasswords = map (fst . head . readP_to_S parseRuleAndPassword) $ lines text
  let validPasswords = filter (uncurry verifyPassword) rulesAndPasswords
  let validPasswords2 = filter (uncurry verifyPassword2) rulesAndPasswords
  print $ length validPasswords
  print $ length validPasswords2
