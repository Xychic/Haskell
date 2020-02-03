import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encode :: Ord a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = [(1 + length same, x)] ++ encode rest
    where (same, rest) = span (==x) xs

encodeSpecial :: Ord a => [a] -> [ListItem a]
encodeSpecial x = map (encodeItem) (encode x)
    where 
        encodeItem (1, a) = Single a
        encodeItem (n, a) = Multiple n a


list = "aaaabccaadeeee"

main :: IO()
main = do
    putStrLn (show (encodeSpecial list))
