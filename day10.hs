import Data.List (group, sort, tails, transpose)

-- parse
parseNumbers :: String -> [Int]
parseNumbers = map read . lines

-- part1
chunkN :: Int -> [a] -> [[a]]
chunkN n ns = filter ((== n) . length) $ transpose $ take n $ tails ns

sortedJolts :: (Num a, Ord a) => [a] -> [a]
sortedJolts ns = [0] ++ sort ns ++ [maximum ns + 3]

getDiffs :: (Num a, Ord a) => [a] -> [a]
getDiffs ns = zipWith (flip (-)) ns' (tail ns')
  where
    ns' = sortedJolts ns

getCounts :: Ord a => [a] -> [(a, Int)]
getCounts diffs = map (\l -> (head l, length l)) $ group $ sort diffs

part1 = getCounts . getDiffs

-- part2
combinations' :: [Int] -> [(Int, Int)]
combinations' [n] = [(n, 1)]
combinations' (n:ns) = (n, count) : ns'
  where
    ns' = combinations' ns
    count = sum $ map snd $ takeWhile ((n + 3 >=) . fst) ns'

combinations :: [Int] -> Int
combinations = snd . head . combinations'

main :: IO ()
main = do
  text <- readFile "day10.txt"
  let nums = parseNumbers text
  let p1 = part1 nums
  print $ (*) <$> lookup 1 p1 <*> lookup 3 p1
  print $ combinations $ sortedJolts nums

-- test cases
test1 :: [Int]
test1 =
  [ 28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3
  ]

test2 :: [Int]
test2 = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
