import System.IO

line :: Int -> Int -> String
line a 0 = ""
line a b = (line (a-1) (b-1)) ++ " " ++ show(a)

triangle 1 = line 1 1
triangle a = 
    let
        b = div (a * (a+1))  2
    in
        triangle (a-1) ++ "\n" ++ (line b a)

main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    let x = (read input::Int)
    putStrLn (triangle x)
