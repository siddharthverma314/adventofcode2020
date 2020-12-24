{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Text as T

type Answers = S.Set Char
type Group = [Answers]

-- readers
readAnswers :: T.Text -> Answers
readAnswers text = S.fromList $ T.unpack text
  
readGroup :: T.Text -> Group
readGroup text = map readAnswers $ filter (/= "") $ T.splitOn "\n" text

readGroups :: T.Text -> [Group]
readGroups text = map readGroup $ T.splitOn "\n\n" text

-- questions
part1 :: [Group] -> Int
part1 groups = sum $ map (S.size . S.unions) groups

part2 :: [Group] -> Int
part2 groups = sum $ map (S.size . foldl1 S.intersection) groups

main :: IO ()
main = do
  text <- readFile "day6_1.txt"
  let groups = readGroups $ T.pack text
  print $ part1 groups
  print $ part2 groups
