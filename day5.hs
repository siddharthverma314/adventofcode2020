{-# LANGUAGE LambdaCase #-}

import Data.Set (Set, fromList, (\\))

data Dir = L | U deriving (Show, Eq)

-- | Main binary search function. Does the heavy lifting of this
-- question.
binarySearch ::
  Int -> -- Total search space
  [Dir] -> -- List of partition directions
  Int -- The returned number
binarySearch 1 [] = 0
binarySearch n (d : ds) = case d of
  U -> h + binarySearch h ds
  L -> binarySearch h ds
  where
    h = n `div` 2

-- | Parse the ticket into row and column number
parseTicket :: String -> (Int, Int)
parseTicket s = (binarySearch 128 rows, binarySearch 8 cols)
  where
    rows =
      map
        ( \c -> case c of
            'F' -> L
            'B' -> U
        )
        (take 7 s)
    cols =
      map
        ( \c -> case c of
            'L' -> L
            'R' -> U
        )
        (drop 7 s)

-- | Get the seatID from a boarding ticket
seatID :: String -> Int
seatID ticket = r * 8 + c
  where
    (r, c) = parseTicket ticket


part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Set Int
part2 tickets = fromList [low..high] \\ fromList tickets
  where
    high = maximum tickets
    low = minimum tickets

main :: IO ()
main = do
  f <- readFile "day5_1.txt"
  let tickets = map seatID $ lines f
  print $ part1 tickets
  print $ part2 tickets
