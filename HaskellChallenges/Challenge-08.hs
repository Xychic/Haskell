import System.IO

compress :: Ord a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = [x] ++ compress (y:xs)

list = "aaaabccaadeeee"

main :: IO()
main = do
    putStrLn (show (compress list))

