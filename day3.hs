import Control.Applicative.Combinators ((<|>))
import Text.ParserCombinators.ReadP

-- data types
data Cell = Open | Tree deriving (Show, Eq)

data Pos = Pos !Int !Int deriving (Show, Eq)

type Grid = [[Cell]]

instance Num Pos where
  (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
  (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)
  abs (Pos x y) = Pos (abs x) (abs y)
  signum (Pos x y) = Pos (signum x) (signum y)
  fromInteger i = Pos (fromInteger i) (fromInteger i)
  negate (Pos x y) = Pos (- x) (- y)

getWidth :: Grid -> Int
getWidth = length . head

getHeight :: Grid -> Int
getHeight = length

getCell :: Grid -> Pos -> Cell
getCell grid (Pos x y) = (grid !! y) !! (x `mod` getWidth grid)

-- parser
parseCell :: ReadP Cell
parseCell = (char '.' >> return Open) <|> (char '#' >> return Tree)

parseRow :: ReadP [Cell]
parseRow = do
  cells <- many1 parseCell
  char '\n'
  return cells

parseGrid :: ReadP Grid
parseGrid = do
  grid <- many parseRow
  eof
  return grid

-- part 1
countTrees :: Pos -> Pos -> Grid -> Int
countTrees start step grid = length $ filter (== Tree) cells
  where
    cells = map (getCell grid) positions
    positions = takeWhile (\(Pos _ y) -> y < getHeight grid) $ iterate (+ step) start

-- questions
part1 :: Grid -> Int
part1 grid = countTrees (Pos 0 0) (Pos 3 1) grid

part2 :: Grid -> Int
part2 grid = product $ map (\s -> countTrees (Pos 0 0) s grid) slopes
  where
    slopes = [Pos 1 1, Pos 3 1, Pos 5 1, Pos 7 1, Pos 1 2]

-- main
main :: IO ()
main = do
  text <- readFile "day3_1.txt"
  let grid = fst $ head $ readP_to_S parseGrid text
  print $ part1 grid
  print $ part2 grid

-- test
testMap =
  fst $
    head $
      readP_to_S parseGrid $
        unlines
          [ "..##.......",
            "#...#...#..",
            ".#....#..#.",
            "..#.#...#.#",
            ".#...##..#.",
            "..#.##.....",
            ".#.#.#....#",
            ".#........#",
            "#.##...#...",
            "#...##....#",
            ".#..#...#.#"
          ]
