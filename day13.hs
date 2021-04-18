{-# LANGUAGE TupleSections #-}

import Data.List (find)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Text.ParserCombinators.ReadP

type Bus = Int

type Time = Int

data Schedule = Schedule
  { time :: Time,
    buses :: [Maybe Bus]
  }
  deriving (Show)

-- parse
parseNumber :: ReadP Int
parseNumber = do
  num <- many1 $ satisfy (`elem` "0123456789")
  return $ read num

parseBus :: ReadP (Maybe Int)
parseBus = (Just <$> parseNumber) +++ (char 'x' >> return Nothing)

parseSchedule :: ReadP Schedule
parseSchedule = do
  time <- parseNumber
  skipSpaces
  buses <- sepBy1 parseBus $ char ','
  skipSpaces
  eof
  return $ Schedule time buses

-- solve
findBus :: Schedule -> (Bus, Time)
findBus schedule = head $ mapMaybe buses [time schedule ..]
  where
    buses t = (,t - time schedule) <$> hasBus schedule t
    hasBus (Schedule _ buses) time = find (\x -> time `mod` x == 0) (catMaybes buses)

-- part 2
inverse :: Int -> Int -> Int
inverse x m = fromJust $ find (\y -> x * y `mod` m == 1) [1 .. m - 1]

-- | Chinese remainder theorem
crt :: [(Int, Int)] -> Int
crt resmod = sum (map ind resmod) `mod` bigm
  where
    bigm = product $ map snd resmod
    ind (a, m) =
      let b = bigm `div` m
          bp = inverse b m
       in a * b * bp

task2 :: Schedule -> Int
task2 (Schedule _ buses) = crt $ catMaybes $ zipWith (\b i -> (- i,) <$> b) buses [0 ..]

-- main
main :: IO ()
main = do
  text <- readFile "day13.txt"
  let schedule = fst $ head $ readP_to_S parseSchedule text
  print $ findBus schedule
  print $ task2 schedule

test = fst $ head $ readP_to_S parseSchedule "939\n7,13,x,x,59,x,31,19\n"
