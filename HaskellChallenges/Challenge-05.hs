import System.IO

getReverse :: Ord a => [a] -> [a]
getReverse (x:xs) 
    | xs == [] = [x]
    | otherwise = (getReverse xs) ++ [x]

list = [1..10]

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (getReverse list))

