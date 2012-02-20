module SudokuSolver (solve, sudoku, isValid) where
import Data.List (group, (\\), sort)
import Data.Maybe (fromMaybe)

sudoku :: [Int]
sudoku = [5,3,4,6,7,0,0,1,2,
          6,7,2,1,9,5,3,4,8,
          0,0,0,0,0,0,5,6,7,
          8,0,0,0,0,1,0,0,3,
          4,2,6,8,5,3,7,9,0,
          7,0,0,9,0,0,0,5,0,
          9,6,1,5,3,7,0,0,0,
          2,8,7,4,1,9,6,0,5,
          3,4,5,2,8,0,1,7,9]

correctSudoku :: [Int]
correctSudoku = [5,3,4,6,7,8,9,1,2,
                 6,7,2,1,9,5,3,4,8,
                 1,9,8,3,4,2,5,6,7,
                 8,5,9,7,6,1,4,2,3,
                 4,2,6,8,5,3,7,9,1,
                 7,1,3,9,2,4,8,5,6,
                 9,6,1,5,3,7,2,8,4,
                 2,8,7,4,1,9,6,3,5,
                 3,4,5,2,8,6,1,7,9] -- for comparison

row :: Int -> [Int] -> [Int]
row y grid = foldl (\acc x -> (grid !! x):acc) [] [y*9 .. y*9+8]
    where y' = y*9
column :: Int -> [Int] -> [Int]
column x grid = foldl (\acc n -> (grid !! n):acc) [] [x,x+9..80]
box :: Int -> Int -> [Int] -> [Int]
box x y grid = foldl (\acc n -> (grid !! n):acc) [] [x+y*9*3+y' | y' <- [0,9,18], x <- [x'..x'+2]]
    where x' = x*3

isValid :: [Int] -> Bool
isValid grid = and [isValidRow, isValidCol, isValidBox]
    where isValidRow = isValidDiv row
          isValidCol = isValidDiv column
          isValidBox = and $ map (\(x,y) -> isValidList (box x y grid)) [(x,y) | x <- [0..2], y <- [0..2]]
--          isValidDiv f = and $ foldl (\acc x -> isValidList (f x grid):acc) [] [0..8]
          isValidDiv f = and $ map (\x -> isValidList (f x grid)) [0..8]
          isValidList = all (\x -> null $ drop 1 x) . group . sort . filter (/= 0) -- tail removes entries that are '0'
        
isComplete :: [Int] -> Bool        
isComplete grid = null (filter (== 0) grid)

solve :: Maybe [Int] -> Maybe [Int]
solve grid' = foldl f Nothing [0..80]
    where grid = fromMaybe [] grid' 
          f acc x
            | isValid grid = if isComplete grid then grid' else f' acc x
            | otherwise    = acc
          f' acc x 
            | (grid !! x) == 0 = case guess x grid of 
                Nothing -> acc
                Just x -> Just x
            | otherwise        = acc

guess :: Int -> [Int] -> Maybe [Int]
guess x grid
    | length valid /= 0 = foldl f Nothing valid
    | otherwise         = Nothing
    where valid = [1..9] \\ (row rowN grid ++ column colN grid ++ box (fst boxN) (snd boxN) grid) -- remove numbers already used in row/collumn/box
          rowN = x `div` 9 -- e.g. 0/9=0 75/9=8
          colN = x - (rowN * 9) -- e.g. 0-0=0 75-72=3
          boxN = (colN `div` 3, rowN `div` 3)
          before x = take x grid
          after x = drop (x+1) grid
          f acc y = case solve $ Just $ before x ++ [y] ++ after x of
            Nothing -> acc
            Just x -> Just x
