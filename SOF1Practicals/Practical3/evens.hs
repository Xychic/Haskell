import System.IO

countEvens :: [Int] -> Int
countEvens [] = 0
countEvens (x:xs)
    | (mod x 2) == 0 = 1 + countEvens xs
    | otherwise = countEvens xs

getEvens :: [Int] -> [Int]
getEvens n = [x | x <- n, (mod x 2) == 0]

toSet :: [Int] -> [Int]
toSet [] = []
toSet (x:xs)
    | x `elem` xs = toSet xs
    | otherwise = x : toSet xs

getUniqueEvens :: [Int] -> [Int]
getUniqueEvens n = getEvens(toSet n)

countUniqueEvens :: [Int] -> Int
countUniqueEvens n = countEvens(toSet n)


main :: IO()
main = do
    putStr "Enter some numbers: "
    hFlush stdout
    input <- getLine
    let x = (map (read::String->Int) (words input))
    putStrLn("There are " ++ show(countEvens(x)) ++ " even numbers: " ++ show(getEvens x))
    putStrLn("There are " ++ show(countUniqueEvens(x)) ++ " distinct even numbers: " ++ show(getUniqueEvens x))