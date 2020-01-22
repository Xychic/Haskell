import System.IO
import Data.Char

sumDigits :: String -> Int
sumDigits "" = 0
sumDigits (x:xs) = (digitToInt x) + (sumDigits xs)

main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    putStrLn (show (sumDigits input))
