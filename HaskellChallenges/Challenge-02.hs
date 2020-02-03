import System.IO

getButLast :: Ord a => [a] -> a
getButLast (x:y:xs)
    | xs == [] = x
    | otherwise = getButLast (y:xs)

list = [1..10]

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (getButLast list))

