import System.IO

add :: [Int] -> [Int] -> [Int]
add (a1:a2:a3:end) (b1:b2:b3:end2) = (a1+b1:a2+b2:a3+b3:end)

mult :: [Int] -> Int -> [Int]
mult a b = map (*b) a

a = [1,2,3]
b = [2,4,6]
main :: IO()
main = do
    putStrLn(show (add a b))
    putStrLn(show (mult a 5))