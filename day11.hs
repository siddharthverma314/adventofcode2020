{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.List (intercalate, unfoldr, find)
import Data.Maybe (fromJust, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP

data Seat = Occupied | Empty deriving (Eq)

data Cell = Seat Seat | Floor deriving (Eq)

data Grid = Grid
  { width :: Int,
    height :: Int,
    cells :: Vector Cell
  }
  deriving (Eq)

cell :: Int -> Int -> Grid -> Maybe Cell
cell x y grid
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

adjacent :: Int -> Int -> Grid -> [Cell]
adjacent x y grid = mapMaybe (\(x, y) -> cell x y grid) pos
  where
    pos :: [(Int, Int)]
    pos = do
      dx <- [-1 .. 1]
      dy <- [-1 .. 1]
      guard $ not $ dx == 0 && dy == 0
      return (x + dx, y + dy)

step :: Grid -> Grid
step g = Grid w h c'
  where
    (Grid w h _) = g

    next :: (Int, Int) -> Cell
    next (x, y)
      | cur == Seat Empty    && adjOccupiedCount == 0 = Seat Occupied
      | cur == Seat Occupied && adjOccupiedCount >= 4 = Seat Empty
      | otherwise = cur
      where
        cur = fromJust $ cell x y g
        adj = adjacent x y g
        adjOccupiedCount = length $ filter (== Seat Occupied) adj

    c' = V.fromList $ do
      y <- [0 .. h - 1]
      x <- [0 .. w - 1]
      return $ next (x, y)

steady :: Grid -> Grid
steady grid = fst $ fromJust $ find (uncurry (==)) pairs
  where
    grids = iterate step grid
    pairs = zip grids (tail grids)

main :: IO ()
main = do
  text <- readFile "day11.txt"
  let grid = fst $ head $ readP_to_S parseGrid text
  print $ V.length $ V.filter (== Seat Occupied) $ cells $ steady grid

-- tests
test1 :: String
test1 = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n"
