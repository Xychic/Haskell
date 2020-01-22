import System.IO


flatten :: [Int] -> [Int]
flatten a 
    | length a == 0 = []
    

array = [1,[2,[3]]]

main :: IO()
main = do
    putStrLn(show(flatten array))