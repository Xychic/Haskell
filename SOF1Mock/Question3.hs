import System.IO

sumAll :: Int -> Int -> Int
sumAll a b = if (b < a) || (a < 0) || (b < 0) then 0
    else if a == b then a
    else (foldl (+) 0 [a..b])

main :: IO()
main = do
    putStr "Enter a lower bound: "
    hFlush stdout
    input <- getLine
    let x = (read input :: Int)
    putStr "Enter an upper bound: "
    input2 <- getLine
    let y = (read input2 :: Int)
    putStrLn ("Sum from " ++ show(x) ++ " to " ++ show(y) ++ " is: " ++ show(sumAll x y))