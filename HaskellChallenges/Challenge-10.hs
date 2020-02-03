import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encode :: Ord a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = [(1 + length same, x)] ++ encode rest
    where (same, rest) = span (==x) xs

list = "aaaabccaadeeee"

main :: IO()
main = do
    putStrLn (show (encode list))
