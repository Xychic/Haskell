import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

dropEvery :: Ord a => [a] -> Int -> [a]
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

list = "abcdefghik"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (dropEvery list 3))
