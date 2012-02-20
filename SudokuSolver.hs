module SudokuSolver (solve, sudoku, prettyPrint) where
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
column :: Int -> [Int] -> [Int]
column x grid = foldl (\acc n -> (grid !! n):acc) [] [x,x+9..80]
box :: Int -> Int -> [Int] -> [Int]
box x y grid = foldl (\acc n -> (grid !! n):acc) [] [x+y*9*3+y' | y' <- [0,9,18], x <- [x'..x'+2]]
    where x' = x*3

isValid = all (\x -> length x <= 9) . group . sort . filter (/= 0)
        
isComplete :: [Int] -> Bool        
isComplete grid = null (filter (== 0) grid)

solve :: Maybe [Int] -> Maybe [Int]
solve grid'
    | isValid grid = if isComplete grid then grid' else f' fold
    | otherwise    = Nothing
    where fold = foldl f (-1,100000000,[1]) [0..80]
          grid = fromMaybe [] grid'
          f acc@(_,l,_) x
            | (grid !! x) == 0 = if validl < l then (x,validl,valid'') else acc
            | otherwise        = acc
            where valid'' = valid' x
                  validl = length valid''
          f' (x,l,valids) = case g x valids of
            Nothing -> Nothing
            Just x -> Just x
          g x valids = guess x grid valids
          valid' x = valid (rowN x) (colN x) (boxN x) grid
          rowN x = getRowN x
          colN x = x - (rowN x) * 9
          boxN x = ((colN x) `div` 3, (rowN x) `div` 3)

guess x grid valids
    | not $ null valids = foldl f Nothing valids
    | otherwise         = Nothing
    where before x = take x grid
          after x  = drop (x+1) grid
          f acc y = case solve $ Just $ before x ++ [y] ++ after x of
            Nothing -> acc
            Just x -> Just x

valid rowN columnN boxN grid = [1..9] \\ (row rowN grid ++ column columnN grid ++ box (fst boxN) (snd boxN) grid) -- remove numbers already used in row/column/box

getRowN x = x `div` 9 -- e.g. 0/9=0 75/9=8
getColN x = x - ((getRowN x) * 9) -- e.g. 0-0=0 75-72=3
getBoxN x = ((getColN x) `div` 3, (getRowN x) `div` 3)

prettyPrint grid = foldl (\acc x -> if (x+1)`mod`9 == 0 then acc ++ (show $ grid !! x)++"\n" else acc ++ (show $ grid !! x) ++ " ") "" [0..80]
