import System.IO
import Data.Char

sumDigits :: String -> Int
sumDigits d 
    | length d == 0 = 0
    | otherwise = digitToInt (head d) + sumDigits (tail d)



main :: IO()
main = do
    putStr "Enter some digits: "
    hFlush stdout
    input <- getLine
    putStrLn(show(sumDigits input))
