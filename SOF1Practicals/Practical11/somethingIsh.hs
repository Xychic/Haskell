import System.IO
import Data.List

-- somethingIsh :: [Char] -> String -> Bool
-- somethingIsh pattern word 
--     | pattern == "" = True
--     | elem (head pattern) word = somethingIsh (tail pattern) word
--     | otherwise = False

somethingIsh :: String -> String ->  Bool
somethingIsh word pattern 
    | pattern == "" = True
    | word == "" = False
    | elem (head word) pattern = somethingIsh (tail word) (delete (head word) pattern)
    | otherwise = somethingIsh (tail word) pattern

elfIsh :: String -> Bool
elfIsh word = somethingIsh word "elf"

replace :: Char -> Char -> String -> String
replace a b word = 
    let 
        repl :: Char -> Char
        repl c 
            | c == a = b
            | otherwise = c
    in
        map repl word

remove :: String -> Char -> String
remove word c = [ x | x <- word, not (elem x [c]) ]


main :: IO()
main = do
    putStr "Enter a word: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter a pattern: "
    hFlush stdout
    input2 <- getLine
    putStrLn(show(somethingIsh input1 input2))
