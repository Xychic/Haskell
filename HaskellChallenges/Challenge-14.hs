import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

dupli :: Ord a => [a] -> [a]
dupli (x:xs) 
    | xs == [] = [x, x]
    | otherwise = [x, x] ++ dupli xs

list = "abccd"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (dupli list))
