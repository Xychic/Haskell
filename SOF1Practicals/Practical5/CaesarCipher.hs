import System.IO
import Data.Char

caesarCipher :: String -> Int -> String
caesarCipher text key = 
    let
        shift :: Char -> Int -> Char
        shift a base = chr((mod ((ord a) - base + key) 26) + base)
    in
        [if 64 < (ord x) && (ord x) < 91 then (shift x 65) 
        else if 96 < (ord x) && (ord x) < 123 then (shift x 97) 
        else x | x <- text]

main :: IO()
main = do
    putStr "Enter text to encrypt/decrypt: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter a key: "
    hFlush stdout
    input2 <- getLine
    let x = (read input2::Int)
    putStrLn (caesarCipher input1 x)
