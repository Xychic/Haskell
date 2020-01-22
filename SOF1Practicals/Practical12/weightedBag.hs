import System.IO

values = [20, 5, 5, 10, 40, 15, 25]
weights = [1, 2, 2, 3, 8, 7, 4]

-- fillBag :: Int -> [Int] -> [Int] -> Int
-- fillBag maxWeight values weights
--     | (length values) == 0
--          = 0
--     | (head weights) <= maxWeight
--          = max (
--             (head values) + (fillBag (maxWeight - (head weights)) (tail values) (tail weights))) 
--             (fillBag maxWeight (tail values) (tail weights))
--     | otherwise 
--         = fillBag maxWeight (tail values) (tail weights)

fillBag :: Int -> [Int] -> [Int] -> Int
fillBag maxWeight (value:valueTail) (weight:weightTail)
    | (length (value:valueTail)) == 0
         = 0
    | weight <= maxWeight
         = max (
            value + (fillBag (maxWeight - weight) valueTail weightTail)) 
            (fillBag maxWeight valueTail weightTail)
    | otherwise 
        = fillBag maxWeight valueTail weightTail


repeatFB :: Int -> IO()
repeatFB n 
    | n <= 0 = putStrLn("0: " ++ " : " ++ show(fillBag n values weights))
    | otherwise = 
        repeatFB (n-1) >>
        putStrLn(show(n) ++ ": " ++ show(fillBag n values weights))

main :: IO()
main = do
    putStr "Enter a max weight for the bag: "
    hFlush stdout
    input <- getLine
    let x = (read input :: Int)
    -- putStrLn( show(fillBag x values weights))
    repeatFB x