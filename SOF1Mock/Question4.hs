import System.IO

scramble :: String -> String
scramble w = if (length w <= 5)
    then w
    else do 
        let start = (take 2 w)
        let end = reverse (take 2 (reverse (w)))
        let mid = take ((length w) - 4) (tail (tail (w)))
        start ++ " " ++ mid ++ " " ++ end

main :: IO()
main = do
    putStr "Enter a word to scramble: "
    hFlush stdout
    word <- getLine
    putStrLn (scramble word)
    putStrLn "What did you expect? It's Haskell! Random is bad."

