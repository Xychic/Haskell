import System.IO

rasterise :: [Int] -> Int -> [[Int]]
rasterise [] b = []
rasterise a b
    | mod (length a) b /= 0 = []
    | otherwise = [(take b a)] ++ (rasterise (drop b a) b)

a = [1,2,3,4,5,6,7,8,9]
main :: IO()
main = do
    putStrLn (show a)
    putStrLn (show (rasterise a 4))
    putStrLn (show (rasterise a 3))