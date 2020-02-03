import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeSpecial :: Ord a => [a] -> [ListItem a]
encodeSpecial [] = []
encodeSpecial (x:xs)
    | len == 0 = [Single x] ++ encodeSpecial rest
    | otherwise = [Multiple len x] ++ encodeSpecial rest
    where 
        (same, rest) = span (==x) xs
        len = 1 + length same 

list = "aaaabccaadeeee"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (encodeSpecial list))
