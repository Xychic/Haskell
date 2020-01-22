import System.IO

toBase10 :: String -> Int
toBase10 "" = 0
toBase10 (x:xs) 
    | x == '1' = (2 ^ (length (x:xs) -1)) + toBase10 xs
    | otherwise = toBase10 xs

main :: IO()
main = do
    putStr "Enter a binary number: "
    hFlush stdout
    input <- getLine
    putStrLn (show (toBase10 input))
