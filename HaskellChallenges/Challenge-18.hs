import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (1+b-a) (drop (a-1) xs)

list = "abcdefghik"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (slice list 3 7))
