import System.IO
import Data.Char

pairWise "" "" = ""
pairWise (x:xs) (y:ys)
    | x == y = "1" ++ pairWise xs ys
    | otherwise = "0" ++ pairWise xs ys

main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter another number: "
    hFlush stdout
    input2 <- getLine
    putStrLn (pairWise input1 input2)
