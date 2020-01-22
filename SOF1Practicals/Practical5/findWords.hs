import System.IO
import Control.Monad

findWords :: [String] -> Char -> [String]
findWords words a = [(x:xs) | (x:xs) <- words, x == a]

findWordsUnique :: [String] -> Char -> [String]
findWordsUnique words a = 
    let 
        toSet :: [String] -> [String]
        toSet [] = []
        toSet (x:xs) 
            | x `elem` xs = toSet xs
            | otherwise = x : toSet xs
    in
        toSet(findWords words a)


main = do
    putStr "Enter a charcter to filter words by: "
    hFlush stdout
    input <- getLine
    handle <- openFile "text.txt" ReadMode
    contents <- hGetContents handle
    let lines = (words contents)
    print (findWords lines (head input))
    print (findWordsUnique lines (head input))
    hClose handle