import SudokuSolver
import Data.Maybe (fromMaybe)

mSudoku :: [Int]
{-mSudoku =[5,0,4,6,7,0,0,1,2,
          0,7,2,1,0,5,3,4,8,
          0,9,0,3,4,0,5,6,7,
          0,0,0,7,0,0,4,0,3,
          4,0,0,0,0,3,7,9,1,
          0,1,0,9,2,0,8,0,6,
          9,0,1,0,0,0,0,8,4,
          0,0,7,4,0,9,6,0,5,
          3,0,0,0,0,0,1,7,0] 
-}
mSudoku = [0,0,0,0,4,0,0,0,9,
           4,0,0,5,0,0,3,8,0,
           8,0,7,9,0,0,0,0,0,
           9,4,0,6,1,0,8,0,5,
           0,0,0,0,9,0,1,0,0,
           0,0,6,8,0,0,4,0,0,
           0,0,1,0,2,9,0,0,0,
           2,0,4,7,0,3,9,5,0,
           6,0,0,0,0,0,2,7,0]
main =
--    print "Hello, world"
    putStrLn . prettyPrint . fromMaybe [] . solve . Just $ mSudoku
--    putStrLn . show . solve' . Just $ sudoku
