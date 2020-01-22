import System.IO

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

a = [[1,2],[3,4,5,6],[7],[8,9]]
main :: IO()
main = do
    putStrLn (show a)
    putStrLn (show (flatten a))
