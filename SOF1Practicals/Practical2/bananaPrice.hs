import System.IO

price :: Float -> Float
price mass
    | (mass * 3) > 50 = 1.5 + (mass * 3)
    | otherwise = 4.99 + (mass * 3)

main :: IO()
main = do
    putStr "Enter a mass of bananas: "
    hFlush stdout
    input <- getLine
    let x = (read input :: Float)
    putStrLn (input ++ "KGs of banans will cost £" ++ show(price x))
