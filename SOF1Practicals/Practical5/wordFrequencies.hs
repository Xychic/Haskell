import System.IO
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

getFrequencies :: [String] -> Map String Int
getFrequencies [] = Map.empty
getFrequencies (x:xs) = Map.unionWith (+) (Map.insert x 1 Map.empty) (getFrequencies xs)

main = do
    handle <- openFile "text.txt" ReadMode
    contents <- hGetContents handle
    let lines = (words contents)
    putStrLn (show (getFrequencies lines))
    hClose handle