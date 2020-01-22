import System.IO

repeatString :: String -> Int -> String
repeatString string 0 = ""
repeatString string n = string ++ repeatString string (n-1)

row :: Int -> Int -> String
row a b = (repeatString "x" a) ++ (repeatString "-" (b-a))

tri :: Int -> Int -> String
tri 1 b = row 1 b
tri a b = (tri (a-1) b) ++ "\n" ++ row a b

triangle :: Int -> String
triangle 1 = "x"
triangle x = if (x < 0)
    then "None"
    else tri x x
                
main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input <- getLine
    let x = (read input :: Int)
    let a = triangle x
    putStrLn (a)