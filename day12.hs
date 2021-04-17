import Data.Char (isDigit)
import Linear.V2
import Text.ParserCombinators.ReadP (ReadP, char, eof, many1, readP_to_S, satisfy, sepBy1, skipSpaces, (+++))
import Prelude hiding (Left, Right)

-- types
data Instr
  = North Int
  | South Int
  | East Int
  | West Int
  | Left Int
  | Right Int
  | Forward Int
  deriving (Show)

data State = State
  { pos :: V2 Int,
    wp :: V2 Int
  }
  deriving (Show)

rot :: Int -> V2 Int -> V2 Int
rot i dir = iterate perp dir !! ((i `div` 90) `mod` 4)

dir :: Int -> V2 Int
dir = flip rot $ V2 1 0

-- parse
parseNum :: ReadP Int
parseNum = do
  digits <- many1 $ satisfy isDigit
  return $ read digits

parseInstr :: ReadP Instr
parseInstr =
  inst 'N' North
    +++ inst 'S' South
    +++ inst 'E' East
    +++ inst 'W' West
    +++ inst 'L' Left
    +++ inst 'R' Right
    +++ inst 'F' Forward
  where
    inst :: Char -> (Int -> Instr) -> ReadP Instr
    inst c i = char c >> i <$> parseNum

parseInstrs :: ReadP [Instr]
parseInstrs = do
  instrs <- sepBy1 parseInstr skipSpaces
  skipSpaces
  eof
  return instrs

-- step
step1 :: Instr -> State -> State
step1 instr (State pos wp) = case instr of
  (North i) -> addPos $ pure i * dir 0
  (East i) -> addPos $ pure i * dir 90
  (South i) -> addPos $ pure i * dir 180
  (West i) -> addPos $ pure i * dir 270
  (Forward i) -> addPos $ pure i * wp
  (Left i) -> addDir (- i)
  (Right i) -> addDir i
  where
    addPos delta = State (pos + delta) wp
    addDir delta = State pos $ rot delta wp

step2 :: Instr -> State -> State
step2 instr (State pos wp) = case instr of
  (North i) -> addDir $ pure i * dir 0
  (East i) -> addDir $ pure i * dir 90
  (South i) -> addDir $ pure i * dir 180
  (West i) -> addDir $ pure i * dir 270
  (Forward i) -> addPos $ pure i * wp
  (Left i) -> rotDir (- i)
  (Right i) -> rotDir i
  where
    addPos delta = State (pos + delta) wp
    addDir delta = State pos $ wp + delta
    rotDir delta = State pos $ rot delta wp

steps :: (Instr -> State -> State) -> [Instr] -> State -> State
steps step instrs start = foldl (flip step) start instrs

showResult :: State -> IO ()
showResult pos = do
  putStr "Final position: "
  print pos
  putStr "Manhattan Distance: "
  print $ let State (V2 a b) _ = pos in abs a + abs b

main :: IO ()
main = do
  text <- readFile "day12.txt"
  let instrs = fst $ head $ readP_to_S parseInstrs text

  showResult $ steps step1 instrs $ State {pos = pure 0, wp = dir 90}
  showResult $ steps step2 instrs $ State {pos = pure 0, wp = V2 1 10}
