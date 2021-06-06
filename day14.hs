{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative (liftA2)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Debug.Trace
import Text.ParserCombinators.ReadP

type Mask = [Maybe Int]

-- parse
data Instr = Mask Mask | Mem {addr :: Int, val :: Int} deriving (Show)

parseBit :: ReadP (Maybe Int)
parseBit =
  (char '0' >> return (Just 0))
    +++ (char '1' >> return (Just 1))
    +++ (char 'X' >> return Nothing)

parseNum :: ReadP Int
parseNum = do
  num <- many1 $ satisfy (`elem` "0123456789")
  return $ read num

parseMask :: ReadP Instr
parseMask = do
  string "mask = "
  bits <- many1 parseBit
  return $ Mask bits

parseMem :: ReadP Instr
parseMem = do
  string "mem["
  addr <- parseNum
  string "] = "
  val <- parseNum
  return $ Mem {addr, val}

parseInstr = do
  instrs <- sepBy1 (parseMem +++ parseMask) skipSpaces
  skipSpaces
  eof
  return instrs

-- | packs bits into an int
bits :: [Int] -> Int
bits xs = foldr1 (\b x -> shiftL x 1 + b) $ reverse xs

-- | unpacks int into bits
unbits :: Int -> [Int]
unbits 0 = []
unbits x = unbits (shiftR x 1) ++ [x .&. 1]

-- | pad a list with a specific item in the start
pad :: a -> Int -> [a] -> [a]
pad item length xs = reverse $ take length $ reverse xs ++ repeat item

-- | cartesian product of lists
cprod :: [[Int]] -> [[Int]]
cprod = foldr (liftA2 (:)) [[]]

-- | takes a mask and applys it onto an integer
applyMask :: (Maybe Int -> Int -> [Int]) -> Mask -> Int -> [Int]
applyMask fn mask num = map bits $ cprod $ zipWith fn mask paddedNum
  where
    paddedNum :: [Int]
    paddedNum = pad 0 (length mask) $ unbits num

-- memory
data MemState = MemState {mem :: M.Map Int Int, mask :: Mask} deriving (Show)

initState :: MemState
initState = MemState M.empty [Nothing]

runInstr1 :: [Instr] -> MemState -> MemState
runInstr1 instrs start =
  foldl
    ( \(MemState mem mask) instr ->
        case instr of
          (Mask newmask) -> MemState mem newmask
          (Mem addr val) ->
            MemState
              (M.insert addr (maskfn mask val) mem)
              mask
    )
    start
    instrs
  where
    maskfn :: Mask -> Int -> Int
    maskfn m x = head $ applyMask (\m x -> [fromMaybe x m]) m x

runInstr2 :: [Instr] -> MemState -> MemState
runInstr2 instrs start =
  foldl
    ( \(MemState mem mask) instr ->
        case instr of
          (Mask newmask) -> MemState mem newmask
          (Mem addr val) ->
            MemState (foldr (`M.insert` val) mem (maskfn mask addr)) mask
    )
    start
    instrs
  where
    maskfn :: Mask -> Int -> [Int]
    maskfn m x =
      applyMask
        ( \m x -> case m of
            Just 0 -> [x]
            Just 1 -> [1]
            Nothing -> [0, 1]
        )
        m
        x

main :: IO ()
main = do
  text <- readFile "day14.txt"
  let instrs = fst $ head $ readP_to_S parseInstr text
  let (MemState mem _) = runInstr1 instrs initState
  print $ sum $ M.elems mem
  let (MemState mem2 _) = runInstr2 instrs initState
  print $ sum $ M.elems mem2
