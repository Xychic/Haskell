import System.IO

data ListItem a = Single a | Multiple Int a
    deriving (Show)

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!(n-1), take (n-1) xs ++ drop n xs)

list = "abcdefgh"

main :: IO()
main = do
    putStrLn (show list)
    putStrLn (show (removeAt 2 list))

