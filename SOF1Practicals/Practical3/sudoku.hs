import System.IO

sudoku = [[0,2,1,4],[3,4,2,1],[1,2,3,4],[0,0,2,3]]

showBoard [] = "+-+-+-+-+"
showBoard (x:xs) = "+-+-+-+-+\n" ++ (showLine x) ++ (showBoard xs)

showLine [] = "|\n"
showLine (x:xs)
    | x == 0 = "| " ++ (showLine xs)
    | otherwise = "|" ++ show(x) ++ (showLine xs)

counter = 4
main :: IO()
main = do
    putStrLn (showBoard sudoku)