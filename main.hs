import SudokuSolver

mSudoku :: [Int]
mSudoku = [0,0,0,0,4,0,0,0,9,
           4,0,0,5,0,0,3,8,0,
           8,0,7,9,0,0,0,0,0,
           9,0,0,0,1,0,8,0,5,
           0,0,0,0,9,0,0,0,0,
           0,0,6,8,0,0,4,0,0,
           0,0,1,0,2,9,0,0,0,
           2,0,4,7,0,0,0,5,0,
           6,0,0,0,0,0,2,7,0]

main = do
--    print "Hello, world"
    putStrLn . show . solve . Just $ mSudoku
    putStrLn . show . solve . Just $ sudoku
