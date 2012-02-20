module SudokuSolver (solve) where
import Data.List (group, (\\), sort)
import Data.Maybe (fromMaybe)

sudoku :: [Int]
sudoku = [5,3,4,6,7,0,9,1,2,
          6,7,2,0,9,5,3,4,8,
          1,0,8,0,0,2,5,6,7,
          8,5,0,7,6,1,4,0,3,
          4,2,6,8,5,3,7,9,1,
          7,1,0,9,2,4,8,5,6,
          0,6,1,5,3,7,2,0,4,
          2,8,7,0,1,9,6,3,5,
          3,4,0,2,8,0,1,7,0]

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

row y grid = foldl (\acc x -> acc ++ [grid !! x]) [] [y*9 .. y*9+8]
column x grid = foldl (\acc n -> acc ++ [grid !! n]) [] [n | n <- [x,x+9..80]]
box x y grid = foldl (\acc n -> acc ++ [grid !! n]) [] [x+y*9*3+y' | y' <- [0,9,18], x <- [x*3..x*3+2]]


isValid grid = and [isValidRow, isValidCol, isValidBox]
    where isValidRow = isValidDiv row
          isValidCol = isValidDiv column
          isValidBox = and $ foldl (\acc (x,y) -> (isValidList (box x y sudoku)):acc) [] [(x,y) | x <- [0..2], y <- [0..2]]
          isValidDiv f = and $ foldl (\acc x -> (isValidList (f x grid)):acc) [] [0..8]
          isValidList xs = and $ map (\x -> length x <= 1) $ group $ sort $ filter (/= 0) xs
        
isComplete grid = and [isValid grid, length (filter (== 0) grid) == 0]

solve :: Maybe [Int] -> Maybe [Int]
solve grid' = foldl f Nothing [0..80]
    where grid = fromMaybe [] grid' 
          f acc x
            | isComplete grid = grid'
            | otherwise       = if (isValid grid) then f' acc x else acc
          f' acc x 
            | (grid !! x) == 0 = case (guess x grid) of 
                Nothing -> acc
                Just x -> Just x
            | otherwise        = acc

guess x grid
    | length valid /= 0 = foldl f Nothing valid
    | otherwise         = Nothing
    where valid = [1..9] \\ (row rowN grid ++ column colN grid ++ box (fst boxN) (snd boxN) grid) -- remove numbers already used in row/collumn/box
          rowN = x `div` 9 -- e.g. 0/9=0 75/9=8
          colN = x - (rowN * 9) -- e.g. 0-0=0 75-72=3
          boxN = (colN `div` 3, rowN `div` 3)
          before x = take (x) grid
          after x = drop (x+1) grid
          f acc y = case (solve $ Just $ before x ++ [y] ++ after x) of
            Nothing -> acc
            Just x -> Just x
