{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Control.Applicative.Combinators ((<|>))

-- Passports
data Passport = Passport
  { birthYear :: Int,
    issueYear :: Int,
    expirationYear :: Int,
    height :: (Int, String),
    hairColor :: (Int, Int, Int),
    eyeColor :: String,
    passportID :: String,
    countryID :: Maybe String
  }
  deriving (Show)

-- Parser
whitespace = ['\n', ' ']

readKeyValue :: ReadP (String, String)
readKeyValue = do
  key <- many $ satisfy (`notElem` whitespace)
  char ':'
  value <- many $ satisfy (`notElem` whitespace)
  satisfy (`elem` whitespace)
  return (key, value)

readPassport :: ReadP (M.Map String String)
readPassport = do
  kvs <- many readKeyValue
  satisfy (`elem` whitespace)
  return $ M.fromList kvs

readPassports :: ReadP [M.Map String String]
readPassports = do
  passports <- many readPassport
  eof
  return passports

-- to validated passport
assert :: (a -> Bool) -> a -> Maybe a
assert f v = if f v then return v else Nothing

validateInRange :: Int -> Int -> String -> Maybe Int
validateInRange low high s = readMaybe s >>= assert (\v -> low <= v && v <= high)

parseHeight :: ReadP (Int, String)
parseHeight = do
  strnum <- many1 $ satisfy isDigit
  units <- string "cm" <|> string "in"
  let valCm = validateInRange 150 193 strnum
  let valIn = validateInRange 59 76 strnum
  num <- if units == "cm" then maybe (fail "cm") pure valCm else maybe (fail "in") pure valIn
  return (num, units)

fromHex :: String -> Int
fromHex s = 16 * digit (head s) + digit (s !! 1)
  where
    digit '0' = 0
    digit '1' = 1
    digit '2' = 2
    digit '3' = 3
    digit '4' = 4
    digit '5' = 5
    digit '6' = 6
    digit '7' = 7
    digit '8' = 8
    digit '9' = 9
    digit 'a' = 10
    digit 'b' = 11
    digit 'c' = 12
    digit 'd' = 13
    digit 'e' = 14
    digit 'f' = 15

parseHairColor :: ReadP (Int, Int, Int)
parseHairColor = do
  char '#'
  r <- count 2 $ satisfy (`elem` "0123456789abcdef")
  g <- count 2 $ satisfy (`elem` "0123456789abcdef")
  b <- count 2 $ satisfy (`elem` "0123456789abcdef")
  return (fromHex r, fromHex g, fromHex b)

eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

toPassport :: M.Map String String -> Maybe Passport
toPassport p = Passport
  <$> (M.lookup "byr" p >>= validateInRange 1920 2002)
  <*> (M.lookup "iyr" p >>= validateInRange 2010 2020)
  <*> (M.lookup "eyr" p >>= validateInRange 2020 2030)
  <*> (do
          hgt <- M.lookup "hgt" p
          res <- listToMaybe $ readP_to_S parseHeight hgt
          return $ fst res)
  <*> (do
          clr <- M.lookup "hcl" p
          res <- listToMaybe $ readP_to_S parseHairColor clr
          return $ fst res)
  <*> (M.lookup "ecl" p >>= assert (`elem` eyeColors))
  <*> (M.lookup "pid" p >>= assert (all isDigit) >>= assert (\l -> length l == 9))
  <*> Just (M.lookup "cid" p)

-- main
part1 :: [M.Map String String] -> Int
part1 = length . filter isValid
  where
    isValid :: M.Map String String -> Bool
    isValid ip = all (`elem` ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]) $ M.keys ip

part2 :: [Maybe Passport] -> Int
part2 = length . filter isJust

main :: IO ()
main = do
  text <- readFile "day4_1.txt"
  let ips = fst $ head $ readP_to_S readPassports text
  let vps = map toPassport ips
  print $ part1 ips
  print $ part2 vps

testInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in\n\n"
