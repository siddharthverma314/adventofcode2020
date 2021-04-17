{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.List (find, intercalate, unfoldr)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V2
import Text.ParserCombinators.ReadP

data Seat = Occupied | Empty deriving (Eq)

data Cell = Seat Seat | Floor deriving (Eq)

data Grid = Grid
  { width :: Int,
    height :: Int,
    cells :: Vector Cell
  }
  deriving (Eq)

cell :: V2 Int -> Grid -> Maybe Cell
cell (V2 x y) grid
  | x < 0 || x >= width grid = Nothing
  | y < 0 || y >= height grid = Nothing
  | otherwise = Just $ cells grid V.! (y * width grid + x)

-- printing and reading
instance Show Seat where
  show Occupied = "#"
  show Empty = "L"

instance Show Cell where
  show (Seat s) = show s
  show Floor = "."

instance Show Grid where
  show (Grid w h c) = intercalate "\n" $ map showRow rows
    where
      rows :: [[Cell]]
      rows = take h $ unfoldr (Just . splitAt w) (V.toList c)

      showRow :: [Cell] -> String
      showRow = intercalate "" . map show

parseChar :: Char -> a -> ReadP a
parseChar c a = do char c; return a

parseSeat :: ReadP Seat
parseSeat = parseChar '#' Occupied +++ parseChar 'L' Empty

parseCell :: ReadP Cell
parseCell = parseChar '.' Floor +++ (Seat <$> parseSeat)

parseRow :: ReadP [Cell]
parseRow = do
  cells <- many1 parseCell
  char '\n'
  return cells

parseGrid :: ReadP Grid
parseGrid = do
  rows <- many1 parseRow
  eof
  let lengths = map length rows
  guard $ all (== head lengths) lengths
  return $
    Grid
      { width = length (head rows),
        height = length rows,
        cells = V.fromList $ foldl1 (++) rows
      }

-- cellular automata
directions :: [V2 Int]
directions = do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  guard $ not $ dx == 0 && dy == 0
  return $ V2 dx dy

checkAdjacent :: V2 Int -> V2 Int -> Grid -> Bool
checkAdjacent pos dir grid =
  case cell (pos + dir) grid of
    Just (Seat Occupied) -> True
    _ -> False

checkLine :: V2 Int -> V2 Int -> Grid -> Bool
checkLine pos dir grid = Just (Seat Occupied) == find (/= Floor) seats
  where
    seats = catMaybes $ takeWhile isJust cells
    cells = (`cell` grid) <$> tail (iterate (+ dir) pos)

step :: (V2 Int -> V2 Int -> Grid -> Bool) -> Int -> Grid -> Grid
step checkDir threshold g = Grid w h c'
  where
    (Grid w h _) = g

    next :: V2 Int -> Cell
    next pos
      | cur == Seat Empty && occupiedCount == 0 = Seat Occupied
      | cur == Seat Occupied && occupiedCount >= threshold = Seat Empty
      | otherwise = cur
      where
        cur = fromJust $ cell pos g
        occupiedCount :: Int
        occupiedCount = sum $ do
          dir <- directions
          let hasOccupied = checkDir pos dir g
          return $ if hasOccupied then 1 else 0

    c' = V.fromList $ do
      y <- [0 .. h - 1]
      x <- [0 .. w - 1]
      return $ next $ V2 x y

steady :: (Grid -> Grid) -> Grid -> Grid
steady step grid = fst $ fromJust $ find (uncurry (==)) pairs
  where
    grids = iterate step grid
    pairs = zip grids (tail grids)

main :: IO ()
main = do
  text <- readFile "day11.txt"
  let grid = fst $ head $ readP_to_S parseGrid text
  print $ sol (step checkAdjacent 4) grid
  print $ sol (step checkLine 5) grid
  where
    sol checkDir grid = V.length $ V.filter (== Seat Occupied) $ cells $ steady checkDir grid

-- tests
test1 :: Grid
test1 = fst $ head $ readP_to_S parseGrid "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n"
