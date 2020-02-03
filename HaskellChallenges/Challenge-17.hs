import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

list = "abcdefghik"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (split list 3))
