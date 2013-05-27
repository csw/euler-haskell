{-# OPTIONS_GHC -XParallelListComp #-}

import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Debug.Trace

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

-- Sudoku solver

--data Pos = Zero | One | Two | Three | Four | Five | Six | Seven | Eight
--         deriving (Enum, Show)

--type Coord = (Pos, Pos)
--           deriving (Enum, Show)

--type Possibles = Bits Int

allPossible :: Int
allPossible = foldl setBit 0 [1..9]

data Cell = Pop Int | Open Int
          deriving (Show)

toCell :: Int -> Cell
toCell 0 = Open allPossible
toCell i
       | 1 <= i && i <= 9 = Pop i
       | otherwise        = error "out of range!"

openCell :: Cell -> Bool
openCell (Open _) = True
openCell _ = False

data Sudoku = Sudoku { board :: (IntMap Cell)
                     , pending :: [Int] }
            deriving (Show)

p96_f = do
  f <- readFile "sudoku.txt"
  putStrLn $ show $ p96 f

p96 :: String -> Int
p96 s = sum corners
    where chunks = chunked 10 $ lines s
          corners = map (p96_one . tail) chunks

p96_one :: [String] -> Int
p96_one lines = sum $ zipWith (*) [100,10,1] $ map numAt [0..2]
    where init = buildSudoku $ parseSudokuLines lines
          Just soln = solveSudoku init
          sBoard = board soln
          numAt i = case (sBoard IntMap.! i) of
                      Pop v -> v

render :: Sudoku -> String
render s = unlines $ chunked 9 chars
    where 
      chars = [toEnum $ (fromEnum '0') + v
                   | (_, Pop v) <- IntMap.toAscList $ board s]
    

chunked :: Int -> [a] -> [[a]]
chunked n = unfoldr chunk
    where chunk [] = Nothing
          chunk l = Just $ splitAt n l

buildSudoku :: [[Int]] -> Sudoku
buildSudoku rows = Sudoku { board=masked, pending=pendingK }
    where
      buildRow :: Int -> [Int] -> [(Int,Cell)]
      buildRow rNum row =
          [(9*rNum+col, toCell v) | v <- row | col <- [0..8]]
      allInit = concat $ zipWith buildRow [0..8] rows
      preset = IntMap.fromList allInit
      (pending, filled) = partition (openCell . snd) $ IntMap.toAscList preset
      Just masked = foldM (\m (c, n) -> maskOut m c n) preset filled
      pendingK = map fst pending

--         Just $ unfoldr (Just . splitAt 9) $ IntMap.toAscList $ board s

solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku s
    | null $ pending s = Just s
    | otherwise        = 
        case mapMaybe solveSudoku $ solveStep s of
          []   -> Nothing
          x:xs -> Just x

solveStep :: Sudoku -> [Sudoku]
solveStep s = 
    let
        curI:pending' = pending s
        Open bv = (board s) IntMap.! curI
        poss = map Pop $ filter (bv `testBit`) [1..9]
        tryBoard p = do
          board' <- maskOut (IntMap.insert curI p (board s)) curI p
          Just Sudoku { board=board', pending=pending' }
    in mapMaybe tryBoard poss
      

maskOut :: (IntMap Cell) -> Int -> Cell -> Maybe (IntMap Cell)
maskOut map0 idx (Pop n) = let
    (row, col) = toCoords idx
    bitMask = allPossible `clearBit` n
    applyMask _ e@(Pop _) = e
    applyMask _ (Open bv) = Open (bv .&. bitMask)
    keys = (rowNInd row col) ++ (colNInd row col) ++ (boxNInd row col)
    mMap = IntMap.fromList $ zip keys (repeat $ Open bitMask)
    updated = IntMap.unionWith applyMask mMap map0
    badCell (Open 0) = True
    badCell _ = False
    in case any badCell $ map (updated IntMap.!) keys of
         True -> Nothing
         False -> Just updated

rowNInd row col = [base + i | i <- [0..8], i /= col]
    where base = 9*(row)

colNInd row col = [9*i + offset | i <- [0..8], i /= row]
    where offset = col

boxNInd row col = [base + 9*r + c | r <- [0..2], c <- [0..2]]
    where r3 n = 3 * (n `div` 3)
          base = boardIdx (r3 row, r3 col)

boardIdx :: (Int,Int) -> Int
boardIdx (row, col) = 9*row + col

toCoords :: Int -> (Int,Int)
toCoords idx = idx `divMod` 9

parseSudokuLines :: [String] -> [[Int]]
parseSudokuLines = map (map (\c -> fromEnum c - fromEnum '0'))

main = p96_f
