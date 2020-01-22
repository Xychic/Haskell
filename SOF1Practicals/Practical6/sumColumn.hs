import System.IO

lengthEquals :: [[Int]] -> Int
lengthEquals [] = 0
lengthEquals (a:b:c)
    | length (a:b) >= 2 = 5
    | otherwise = 1

a =  [[1,2,3], [4,5,6], [7,8,9]]
b =  [[1,2,3], [4], [7,8,9]]

main :: IO()
main = do
    -- putStrLn (show (lengthEquals a))
    -- putStrLn (show (lengthEquals b))
    putStrLn (show (lengthEquals [[4]]))