import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

repli :: Ord a => [a] -> Int -> [a]
repli (x:xs) n 
    | xs == [] = replicate n x
    | otherwise = replicate n x ++ repli xs n

list = "abccd"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (repli list 3))
