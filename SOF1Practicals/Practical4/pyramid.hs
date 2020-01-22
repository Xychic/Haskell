import System.IO
import Data.Char

pyramid :: Int -> String
pyramid a = 
    let
        halfLine :: Int -> String
        halfLine 1 = "A"
        halfLine n = halfLine (n-1) ++ " " ++ [chr (64 + n)]

        line :: Int -> String
        line n = halfLine (n) ++ " " ++ reverse (halfLine (n - 1))

        pyramid_ :: Int -> Int -> String
        pyramid_ a b
            | a == 1 = (concat (replicate (b - 1) "  ")) ++ "A"
            | otherwise = (pyramid_ (a-1) b) ++ "\n" ++ (concat (replicate (b - a) "  ")) ++ line a
    in
        pyramid_ a a


main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    let x = (read input::Int)
    putStrLn (pyramid x)