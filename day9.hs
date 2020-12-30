import Data.Function ((&))
import Data.List (find, tails, transpose)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Set as S

-- parsing
parseNumbers :: String -> [Int]
parseNumbers = map read . lines

-- solution
withN :: Int -> [Int] -> [(Int, S.Set Int)]
withN n nums =
  tails nums
    & take (n + 1)
    & transpose
    & takeWhile ((== n + 1) . length)
    & map (\l -> (last l, S.fromList $ init l))

checkTwoSum :: Int -> S.Set Int -> Maybe (Int, Int)
checkTwoSum n ns = find (\(i, j) -> j `S.member` ns && i /= j) allTuples
  where
    allTuples :: [(Int, Int)]
    allTuples = map (\i -> (i, n - i)) $ S.toList ns

firstNonTwoSum :: Int -> [Int] -> Maybe Int
firstNonTwoSum n ns = fst <$> find (isNothing . uncurry checkTwoSum) (withN n ns)

findContiguousSum :: Int -> [Int] -> Maybe [Int]
findContiguousSum n ns =
  tails ns
  & map (scanl1 (+))
  & map (takeWhile (<= n))
  & filter (not . null)
  & find ((== n) . last)
  & fmap (\l -> zipWith (-) l (0 : l))

main :: IO ()
main = do
  text <- readFile "day9.txt"
  let nums = parseNumbers text
  let sum = fromJust $ firstNonTwoSum 25 nums
  print sum
  print $ (\l -> minimum l + maximum l) <$> findContiguousSum sum nums

test :: [Int]
test =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]
