import System.IO
import Data.Char

caesarCipher :: String -> Int -> String
caesarCipher text key = 
    let
        shift :: Char -> Int -> Char
        shift a base = chr((mod ((ord a) - base + key) 26) + base)
    in
        [if 64 < (ord x) then (shift x 65) else if 96 < (ord x) then (shift x 97) else x | x <- text]


text = "bpm owwl vmea ijwcb kwuxcbmza qa bpib bpmg lw epib gwc bmtt bpmu bw lw. bpm jil vmea qa bpib bpmg lw epib gwc bmtt bpmu bw lw"

loop :: Int -> String
loop 0 = ""
loop n = loop (n-1) ++ "\n" ++ show(n) ++ " : " ++ (caesarCipher text n)

main :: IO()
main = do
    putStrLn (loop 26)
