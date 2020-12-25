import Data.Char (isAlphaNum, isDigit)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

type Color = String

type Graph a = M.Map a (S.Set a)

type WeightedGraph a = M.Map a (S.Set (a, Int))

data Rule = Rule Color Color Int deriving (Show)

-- rule parser
parseWord :: ReadP String
parseWord = munch isAlphaNum

parseColor :: ReadP Color
parseColor = do
  w1 <- parseWord
  skipSpaces
  w2 <- parseWord
  return $ unwords [w1, w2]

parseNumber :: ReadP Int
parseNumber = read <$> many1 (satisfy isDigit)

parseContains :: ReadP [(Color, Int)]
parseContains = parseEmpty +++ parseMany
  where
    parseEmpty :: ReadP [(Color, Int)]
    parseEmpty = do
      string "no other bags."
      return []

    parseMany :: ReadP [(Color, Int)]
    parseMany = do
      r <- sepBy parseColorNum (char ',' >> skipSpaces)
      char '.'
      return r

    parseColorNum :: ReadP (Color, Int)
    parseColorNum = do
      n <- parseNumber
      skipSpaces
      c <- parseColor
      skipSpaces
      string "bag" +++ string "bags"
      return (c, n)

parseRule :: ReadP [Rule]
parseRule = do
  from <- parseColor
  skipSpaces
  string "bags contain"
  skipSpaces
  map (uncurry (Rule from)) <$> parseContains

parseRules :: ReadP [Rule]
parseRules = do
  rules <- sepBy parseRule skipSpaces
  skipSpaces
  eof
  return $ foldl1 (++) rules

-- solutions
invert :: [Rule] -> Graph Color
invert = M.fromListWith S.union . map formatRule
  where
    formatRule (Rule c1 c2 _) = (c2, S.singleton c1)

weighted :: [Rule] -> WeightedGraph Color
weighted = M.fromListWith S.union . map formatRule
  where
    formatRule (Rule c1 c2 n) = (c1, S.singleton (c2, n))

dfs :: Ord a => Graph a -> a -> S.Set a
dfs graph = dfsHelper S.empty
  where
    dfsHelper explored node =
      let newExplored = S.insert node explored
          children = fromMaybe S.empty $ graph M.!? node
       in S.foldl
            (\explored child -> S.union explored (dfsHelper explored child))
            newExplored
            children

numBags :: WeightedGraph Color -> Color -> Int
numBags graph node = 1 + sum (map numBagsPerChild children)
  where
    numBagsPerChild (child, count) = count * numBags graph child
    children = maybe [] S.toList (graph M.!? node)

bothParts :: String -> (Int, Int)
bothParts text = (part1, part2)
  where
    rules = lines text >>= fst . head . readP_to_S parseRules
    graph1 = invert rules
    graph2 = weighted rules
    part1 = S.size (dfs graph1 "shiny gold") - 1
    part2 = numBags graph2 "shiny gold" - 1

main :: IO ()
main = do
  text <- readFile "day7_1.txt"
  let (part1, part2) = bothParts text
  print part1
  print part2

test1 =
  unlines
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
      "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
      "bright white bags contain 1 shiny gold bag.",
      "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
      "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
      "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
      "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
      "faded blue bags contain no other bags.",
      "dotted black bags contain no other bags."
    ]

test2 =
  unlines
    [ "shiny gold bags contain 2 dark red bags.",
      "dark red bags contain 2 dark orange bags.",
      "dark orange bags contain 2 dark yellow bags.",
      "dark yellow bags contain 2 dark green bags.",
      "dark green bags contain 2 dark blue bags.",
      "dark blue bags contain 2 dark violet bags.",
      "dark violet bags contain no other bags."
    ]

test3 =
  unlines
    [ "shiny gold bags contain 1 green green bag, 1 red red bag.",
      "green green bags contain 1 blue blue bag.",
      "red red bags contain 1 blue blue bag.",
      "blue blue bags contain no other bags."
    ]
