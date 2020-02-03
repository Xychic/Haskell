import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

rotate :: [a] -> Int -> [a]
rotate xs n
    | 0 < n && n < length xs= drop n xs ++ take n xs
    | otherwise = rotate xs (mod n (length xs))

list = "abcdefgh"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (rotate list 3))
    putStrLn (show (rotate list (-2)))

