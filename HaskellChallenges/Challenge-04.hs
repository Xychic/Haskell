import System.IO

getLength :: Ord a => [a] -> Int
getLength (x:xs) 
    | xs == [] = 0
    | otherwise = 1 + getLength xs

list = [1..10]

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (getLength list))

