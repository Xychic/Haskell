import System.IO
import Data.Char

camelCase :: String -> String
camelCase a = [x | x <- (unwords (camelCase2 a)), x /= ' ']

camelCase2 :: String -> [String]
camelCase2 a = 
    let 
        capitalise (head:tail) = toUpper head : map toLower tail
    in
        map capitalise (words a)

main :: IO()
main = do
    putStr "Enter a sentence: "
    hFlush stdout
    input <- getLine
    putStrLn ([x | x <- input, x /= ' '])
    putStrLn (camelCase input)
    putStrLn (show(camelCase2 input))