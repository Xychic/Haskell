import System.IO

row :: Int -> Int -> String
row a 1 = if (mod a 2) == 0
    then "-"
    else "x"

row a b = if (mod (a + b) 2) == 0
    then "x" ++ row a (b-1)
    else "-" ++ row a (b-1)


g :: Int -> Int -> String
g 1 b = row 1 b
g a b = row a b ++ "\n" ++ (g (a-1) b)


grid :: Int -> String
grid a | a < 2 = "None"
grid a | a >= 2 = g a a 

main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    let x = (read input :: Int)
    putStrLn (grid x)