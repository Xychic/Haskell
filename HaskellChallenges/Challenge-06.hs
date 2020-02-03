import System.IO

isPalindrome :: Ord a => [a] -> Bool
isPalindrome x = x == reverse x

list = [1..6] ++ [5,4..1]
list2 = [1..10]

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (isPalindrome list))
    putStrLn (show list2)
    putStrLn (show (isPalindrome list2))

