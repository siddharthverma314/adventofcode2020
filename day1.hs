power :: Int -> [Int] -> [[Int]]
power 0 _ = [[]]
power n lst = [a : x | a <- lst, x <- power (n - 1) lst]

findNSum :: Int -> Int -> [Int] -> [Int]
findNSum n a lst = head $ filter (\n -> sum n == a) $ power n lst

find2Sum = findNSum 2
find3Sum = findNSum 3

main :: IO ()
main = let
  nums :: IO [Int]
  nums = do
    file <- readFile "day1_1.txt"
    return $ map read $ lines file
  in
  do
    ns <- nums
    print (product $ find2Sum 2020 ns)
    print (product $ find3Sum 2020 ns)
