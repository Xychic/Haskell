import System.IO

mergeSorted :: [Int] -> [Int] -> [Int]
mergeSorted a b 
    | length a == 0 = b
    | length b == 0 = a
    | head a < head b = [head a] ++ mergeSorted (tail a) b
    | otherwise = [head b] ++ mergeSorted a (tail b)

array1 = [1,2,4,5,6,7,8]
array2 = [2,5,7,9]

main :: IO()
main = do
    putStrLn(show(mergeSorted array1 array2))