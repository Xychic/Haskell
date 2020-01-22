import System.IO

interval :: Float -> Float -> String
interval age rate = 
    let 
        m = (208 - (0.7 * age))
    in 
        if (rate < 0.5 * m) then "Couch Potato" else
        if (rate < 0.7 * m) then "Aerobic training" else
        if (rate < 0.9 * m) then "Threshold training" 
        else "Interval training"


main :: IO()
main = do
    putStr "Enter your age: "
    hFlush stdout
    input1 <- getLine
    putStr "Enter your heart rate: "
    hFlush stdout
    input2 <- getLine
    let 
        x = (read input1 :: Float)
        y = (read input2 :: Float)
    putStrLn ("With a heart rate of " ++ input2 ++ " at " ++ input1 ++ " years old you are working at " ++ (interval x y))
