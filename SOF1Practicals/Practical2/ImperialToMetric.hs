import System.IO

impToMet :: Float -> Float -> Float
impToMet stones pounds = (6.35029318 * stones) + (0.453592 * pounds)

main :: IO()
main = do
    putStr "Enter a number of stones: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter a number of pounds: "
    hFlush stdout
    input2 <- getLine
    let
        x = (read input1 :: Float)
        y = (read input2 :: Float)
    putStrLn (input1 ++ "\"" ++ input2 ++ "' is: " ++ show(impToMet x y) ++ "KGs.")