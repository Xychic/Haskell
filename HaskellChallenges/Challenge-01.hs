import System.IO

getLast :: Ord a => [a] -> a
-- getLast (x:xs) = (x:xs)!!length(xs)
getLast (x:xs)
    | xs == [] = x
    | otherwise = getLast xs

list = [1..10]

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (getLast list))
