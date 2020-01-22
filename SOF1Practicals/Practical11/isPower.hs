import System.IO

isPower :: Float -> Float -> Bool
isPower a b 
    | (0 <= a && a < 1) = False
    | a == 1 = True
    | a >= b = isPower (a/b) b
    | otherwise = False



main :: IO()
main = do
    putStr "Enter a number: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter another number: "
    hFlush stdout
    input2 <- getLine
    let x = (read input1 :: Float)
        y = (read input2 :: Float)
    putStrLn (show(isPower x y))
