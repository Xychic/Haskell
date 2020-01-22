import System.IO

rice = 
    let
        count 0 a = a
        count n a = count (n-1) (2*a)
    in
        count 64 30e-3


main = do
    putStrLn (show rice)