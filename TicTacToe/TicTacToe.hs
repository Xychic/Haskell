import System.IO


showBoard :: [[Char]] -> String
showBoard [[a,b,c], [d,e,f], [g,h,i]] = 
    "     0 1 2\n" ++ 
    "   0 " ++ [(if a == '.' then ' ' else a), '│', (if b == '.' then ' ' else b), '│', (if c == '.' then ' ' else c)] ++ "\n    ──┼─┼──\n" ++
    "   1 " ++ [(if d == '.' then ' ' else d), '│', (if e == '.' then ' ' else e), '│', (if f == '.' then ' ' else f)] ++ "\n    ──┼─┼──\n" ++ 
    "   2 " ++ [(if g == '.' then ' ' else g), '│', (if h == '.' then ' ' else h), '│', (if i == '.' then ' ' else i)]


checkWinner :: [[Char]] -> Char
checkWinner [[a,b,c], [d,e,f], [g,h,i]]
    -- Horizontal 
    | a == b && b == c && a /= '.' = a  
    | d == e && e == f && d /= '.' = d
    | g == h && h == i && g /= '.' = h
    -- Vertical
    | a == d && d == g && a /= '.' = a
    | b == e && e == h && b /= '.' = b
    | c == f && f == i && c /= '.' = c
    -- Diagonal
    | a == e && e == i && a /= '.' = a
    | c == e && e == g && c /= '.' = c
    -- Game not over
    | '.' `elem` [a,b,c,d,e,f,g,h,i] = '?'
    -- Tie
    | otherwise = '-'


replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a : xs
replace n a (x:xs) = x : (replace (n-1) a xs)


play :: Int -> Int -> a -> [[a]] -> [[a]]
play r c a board = replace r (replace c a (board !! r)) board


value :: Char -> [[Char]] -> Int
value player board
    | w == 'X' = 1
    | w == 'O' = -1
    | w == '-' = 0
    | player == 'X' = maximum [value 'O' (play r c 'X' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
    | otherwise = minimum [value 'X' (play r c 'O' board) | r <- [0..2], c <- [0..2], board !! r !! c == '.']
    where w = checkWinner board


bestOf :: [[[Char]]] -> [[Char]]
bestOf [x] = x
bestOf (x:xs)
    | value 'O' x > value 'O' (bestOf xs) = x
    | otherwise = bestOf xs 


bestMove :: [[Char]] -> [[Char]]
bestMove board = bestOf [play r c 'X' board | r <- [0..2], c <- [0..2], board !! r !! c == '.']


split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s


getMove :: [Char] -> (Int, Int)
getMove a = (m!!0, m!!1)
    where m = [read c :: Int | c <- (split ' ' a)]


onePlayer :: [[Char]] -> Char -> IO()
onePlayer board player
    | winner == '-' = putStrLn ((showBoard board) ++ "\nIt's a tie!")
    | winner /= '?' = putStrLn ((showBoard board) ++ "\n" ++ [winner] ++ " wins the game!")
    | player == 'X' = onePlayer (bestMove board) 'O'
    | otherwise = do
        putStrLn (showBoard board)
        putStr "Enter a move (y x): "
        hFlush stdout
        input <- getLine
        let (r,c) = getMove input
        if (board !! r !! c) == '.' then
            let board2 = (play r c 'O' board)
            in onePlayer board2 'X'
        else do
            putStrLn "\nSpace already taken!"
            onePlayer board 'O'
    where winner = checkWinner board


twoPlayer :: [[Char]] -> Char -> IO()
twoPlayer board player
    | winner == '-' = putStrLn ((showBoard board) ++ "\nIt's a tie!")
    | winner /= '?' = putStrLn ((showBoard board) ++ "\n" ++ [winner] ++ " wins the game!")        
    | otherwise = do
        putStrLn (showBoard board)
        putStr ("Player '" ++ [player] ++ "' enter a move (y x): ")
        hFlush stdout
        input <- getLine
        let (r,c) = getMove input
        if (board !! r !! c) == '.' then
            let board2 = (play r c player board)
            in if (player == 'O') then (twoPlayer board2 'X') else (twoPlayer board2 'O')
        else do
            putStrLn "\nSpace already taken!"
            twoPlayer board player
    where winner = checkWinner board


emptyBoard = [
    "...",
    "...",
    "..."]


main :: IO()
main = do
    putStr "How many players (1, 2): "
    hFlush stdout
    input <- getLine
    if input == "1" then onePlayer emptyBoard 'O'
    else if input == "2" then twoPlayer emptyBoard 'O' 
    else do 
        putStrLn "Invalid input!\n"
        main