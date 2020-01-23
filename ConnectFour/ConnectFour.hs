import System.IO

showBoard :: [[Char]] -> [Char]
showBoard [a,b,c,d,e,f] = " 0 1 2 3 4 5 6 \n" ++
    createRow a ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow b ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow c ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow d ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow e ++ "\n──┼─┼─┼─┼─┼─┼──\n" ++
    createRow f ++ "\n──┴─┴─┴─┴─┴─┴──\n"

createRow :: [Char] -> [Char]
createRow [a,b,c,d,e,f,g] = " "
    ++ [getSymbol a, '│', getSymbol b, '│', getSymbol c, '│', getSymbol d, '│', getSymbol e, '│', getSymbol f, '│', getSymbol g]
    

getSymbol :: Char -> Char
getSymbol a = if a == '.' then ' ' else a


checkWinner :: [[Char]] -> Char
checkWinner board = 
    let
        check1 :: [[Char]] -> Int -> Int -> Char
        check1 board x y 
            | board!!y!!x == board!!(y+1)!!(x+1) && board!!(y+1)!!(x+1) == board!!(y+2)!!(x+2) && board!!(y+2)!!(x+2) == board!!(y+3)!!(x+3) && board!!y!!x /= '.' = board!!y!!x
            | board!!(y+3)!!x == board!!(y+2)!!(x+1) && board!!(y+2)!!(x+1) == board!!(y+1)!!(x+2) && board!!(y+1)!!(x+2) == board!!y!!(x+3) && board!!(y+3)!!x /= '.' = board!!(y+3)!!x
            | otherwise = '?'

        check2 :: [[Char]] -> Int -> Int -> Char
        check2 board x y
            | board!!y!!x == board!!(y+1)!!x && board!!(y+1)!!x == board!!(y+2)!!x && board!!(y+2)!!x == board!!(y+3)!!x && board!!y!!x /= '.' = board!!y!!x
            | otherwise = '?'
        
        check3 :: [[Char]] -> Int -> Int -> Char
        check3 board x y
            | board!!y!!x == board!!y!!(x+1) && board!!y!!(x+1) == board!!y!!(x+2) && board!!y!!(x+2) == board!!y!!(x+3) && board!!y!!x /= '.' = board!!y!!x
            | otherwise ='?'

        wins = filter (/='?') [
            check1 board 0 0, check1 board 0 1, check1 board 0 2,
            check1 board 1 0, check1 board 1 1, check1 board 1 2,
            check1 board 2 0, check1 board 2 1, check1 board 2 2,
            check1 board 3 0, check1 board 3 1, check1 board 3 2,
            check2 board 0 0, check2 board 0 1, check2 board 0 2,
            check2 board 1 0, check2 board 1 1, check2 board 1 2,
            check2 board 2 0, check2 board 2 1, check2 board 2 2,
            check2 board 3 0, check2 board 3 1, check2 board 3 2,
            check3 board 0 0, check3 board 0 1, check3 board 0 3, check3 board 0 4, check3 board 0 5,
            check3 board 1 0, check3 board 1 1, check3 board 1 3, check3 board 1 4, check3 board 1 5,
            check3 board 2 0, check3 board 2 1, check3 board 2 3, check3 board 2 4, check3 board 2 5,
            check3 board 3 0, check3 board 3 1, check3 board 3 3, check3 board 3 4, check3 board 3 5
            ]        
    in 
        if length wins > 0 then wins!!0
        else if '.' `elem` board!!0 ++ board!!1 ++ board!!2 ++ board!!3 ++ board!!4 ++ board!!5 then '?' else '-'


checkPlay :: [[Char]] -> Int -> Int -> (Bool, Int)
checkPlay board x y
    | y == 0 = (board!!y!!x == '.', 0)
    | board!!y!!x == '.' = (True, y)
    | otherwise = checkPlay board x (y-1)


play :: Int -> Int -> a -> [[a]] -> [[a]]
play x y a board = replace y (replace x a (board !! y)) board


value :: Char -> [[Char]] -> Int
value player board
    | w == 'X' = 1
    | w == 'O' = -1
    | w == '-' = 0
    | player == 'X' = maximum [value 'O' (play x y 'X' board) | x <- [0..6], (valid, y) <- [(checkPlay board x ((length board)-1))], valid==True]
    | otherwise = minimum [value 'X' (play x y 'O' board) | x <- [0..6], (valid, y) <- [(checkPlay board x ((length board)-1))], valid==True]
    where w = checkWinner board


bestOf :: [[[Char]]] -> [[Char]]
bestOf [x] = x
bestOf (x:xs)
    | value 'O' x > value 'O' (bestOf xs) = x
    | otherwise = bestOf xs 


bestMove :: [[Char]] -> [[Char]]
bestMove board = bestOf [play x y 'X' board | x <- [0..6], (valid, y) <- [(checkPlay board x ((length board)-1))], valid==True]


replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a : xs
replace n a (x:xs) = x : (replace (n-1) a xs)


onePlayer :: [[Char]] -> Char -> IO()
onePlayer board player
    | winner == '-' = putStrLn ((showBoard board) ++ "\nIt's a tie!")
    | winner /= '?' = putStrLn ((showBoard board) ++ "\n" ++ [winner] ++ " wins the game!")
    | player == 'X' = onePlayer (bestMove board) 'O'
    | otherwise = do
        putStrLn (showBoard board)
        putStr "Enter a column to play: "
        hFlush stdout
        input <- getLine
        let 
            x = (read input::Int)
            (valid, y) = checkPlay board x ((length board)-1)
        if valid then
            let board2 = (play x y 'O' board)
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
        putStr ("Player '" ++ [player] ++ "' enter a column to play: ")
        hFlush stdout
        input <- getLine
        let 
            x = (read input::Int)
            (valid, y) = checkPlay board x ((length board)-1)
        if valid then
            let board2 = (play x y player board)
            in if (player == 'O') then (twoPlayer board2 'X') else (twoPlayer board2 'O')
        else do
            putStrLn "\nSpace already taken!"
            twoPlayer board player
    where winner = checkWinner board

emptyBoard = [
    ".......",
    ".......",
    ".......",
    ".......",
    ".......",
    "......."]

showMany (x:xs) 
    | x == [] = ""
    | otherwise = (showBoard x) ++ "\n\n" ++ showMany xs

main :: IO()
main = do
    twoPlayer emptyBoard 'X'