import System.IO

line :: Int -> String
line 0 = ""
line n 
    | (mod n 2) == 1 = "1" ++ line (n-1)
    | otherwise = "0" ++ line (n-1)

floydsTriangle :: Int -> String
floydsTriangle 1 = line 1
floydsTriangle n = (floydsTriangle (n-1)) ++ "\n" ++ (line n)

main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    let x = (read input::Int)
    putStrLn (floydsTriangle x)

