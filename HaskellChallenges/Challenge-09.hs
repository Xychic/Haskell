import System.IO

pack :: Ord a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x: same): pack rest
    where (same, rest) = span (==x) xs

list = "aaaabccaadeeee"

main :: IO()
main = do
    putStrLn (show (pack list))

