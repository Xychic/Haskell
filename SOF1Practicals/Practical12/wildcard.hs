import System.IO

wildCardPattern :: String -> Char -> [String]
wildCardPattern pattern wildcard 
    | (length pattern) == 0 = [""]
    | (head pattern) /= wildcard = [[(head pattern)] ++ c | c <- wildCardPattern (tail pattern) wildcard]
    | otherwise = ["0" ++ c | c <- wildCardPattern (tail pattern) wildcard] ++ 
        ["1" ++ c | c <- wildCardPattern (tail pattern) wildcard]

main :: IO()
main = do
    putStr "Enter a pattern: "
    hFlush stdout
    input <- getLine
    putStrLn (show(wildCardPattern input '?'))