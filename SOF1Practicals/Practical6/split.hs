import System.IO

splitOn :: String -> Char -> [String]
splitOn [] c = []
splitOn text c = 
    let (x,y) = span (/= c) text
    in x : (splitOn (drop 1 y) c)

split :: String -> [String]
split a = splitOn a ' '

main :: IO()
main = do
    putStr "Enter some text: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter character to split on: "
    hFlush stdout
    input2 <- getLine
    putStrLn (show (splitOn input1 (head input2)))