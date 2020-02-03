import System.IO

elementAt :: Ord a => [a] -> Int -> a
elementAt (x:xs) pos
    | pos == 1 = x
    | otherwise = elementAt xs (pos-1)

list = [1..10]

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (elementAt list 3))

