{-# LANGUAGE LambdaCase #-}

import Data.Char
import Data.Function
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

data State = State {ip :: Int, reg :: Int} deriving (Show)

data Instr = Acc Int | Jmp Int | Nop Int deriving (Show)

type Program = [Instr]

-- parser
parseInt :: ReadP Int
parseInt = do
  c <- char '+' +++ char '-'
  d <- many1 $ satisfy isDigit
  let sign = if c == '+' then 1 else -1
  return $ sign * read d

parseInstr :: ReadP Instr
parseInstr = acc +++ jmp +++ nop
  where
    readIntInstr instr str = string str >> skipSpaces >> instr <$> parseInt
    acc = readIntInstr Acc "acc"
    jmp = readIntInstr Jmp "jmp"
    nop = readIntInstr Nop "nop"

parseProgram :: ReadP Program
parseProgram = do
  program <- sepBy parseInstr skipSpaces
  skipSpaces
  eof
  return program

-- simulate
runInstr :: Program -> State -> State
runInstr program state =
  case curInstr of
    Acc n ->
      State
        { ip = ip state + 1,
          reg = reg state + n
        }
    Jmp n ->
      State
        { ip = ip state + n,
          reg = reg state
        }
    Nop _ ->
      State
        { ip = ip state + 1,
          reg = reg state
        }
  where
    curInstr = program !! ip state

run :: Program -> State -> [State]
run = iterate . runInstr

terminated :: Program -> State -> Bool
terminated program state = n < 0 || n >= length program
  where
    n = ip state

runUntilLoopOrTerminate :: Program -> State -> State
runUntilLoopOrTerminate program startState =
  zip (tail states) ips
    & dropWhile (\(state, set) -> ip state `S.notMember` set
                  && not (terminated program state))
    & head
    & fst
  where
    states :: [State]
    states = run program startState

    ips :: [S.Set Int]
    ips = scanl (flip $ S.insert . ip) S.empty states

allChangedPrograms :: Program -> [Program]
allChangedPrograms program =
  mapMaybe
    ( \case
        (l, Jmp n, r) -> Just $ l ++ [Nop n] ++ r
        (l, Nop n, r) -> Just $ l ++ [Jmp n] ++ r
        (_, Acc _, _) -> Nothing
    )
    allPrograms
  where
    split3 i = (take i program, program !! i, drop (i + 1) program)
    allPrograms = map split3 [1 .. (length program - 1)]

allTerminatedPrograms :: Program -> State -> [State]
allTerminatedPrograms programs startState =
  allChangedPrograms programs
  & map (\p -> (p, runUntilLoopOrTerminate p startState))
  & filter (uncurry terminated)
  & map snd

main :: IO ()
main = do
  text <- readFile "day8.txt"
  let program = fst $ head $ readP_to_S parseProgram text
  let breakState = runUntilLoopOrTerminate program (State 0 0)
  let modifiedStates = allTerminatedPrograms program (State 0 0)
  print breakState
  print modifiedStates

test :: String
test =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]
