import System.IO

area :: Float -> Float -> Float -> Float
area a b c = 
    let 
        s = (a + b + c) / 2
    in 
        sqrt (s * (s-a) * (s-b) * (s-c))

main :: IO()
main = do
    putStr "Enter length A:  "
    hFlush stdout
    input1 <- getLine
    putStr "Enter length B:  "
    hFlush stdout
    input2 <- getLine
    putStr "Enter length C:  "
    hFlush stdout
    input3 <- getLine
    let 
        x = (read input1 :: Float)
        y = (read input2 :: Float)
        z = (read input3 :: Float)
    putStrLn ("Area of triangle with sides of lengths " ++ input1 ++ "," ++ input2 ++ "," ++ input3 ++ " is " ++ show(area x y z))
