import Utils
import System.IO
import Data.List.Split.Internals
import Data.List
import Data.Matrix
import Basic

main = do
    raw <- readFile "sudoku.txt"
    putStrLn . toString . solver . format $ raw

format :: String -> Sudoku
format = fromList 9 9 . map read . filter (\ x -> all (x/=) ["\n"," ","(",")"]) . chunksOf 1

toString :: Maybe Sudoku -> String
toString Nothing = "No existe solución!"
toString (Just s) = prettyMatrix s

solver :: Sudoku -> Maybe Sudoku
solver s
    | finished s = Just s
    | not $ valid s = Nothing
    | otherwise = ofv guess availables s 

guess :: Sudoku -> [(Int,Int)] -> Maybe Sudoku
guess s [] = if valid s then Just s else Nothing
guess s (a:as) = if impos s a then Nothing else (guesser s a $ posibles s a) >>= flip guess as   

guesser :: Sudoku -> (Int, Int) -> [Int] -> Maybe Sudoku
guesser _ _ [] = Nothing
guesser game pos (p:ps) 
    | game ! pos /= 0 = Just game 
    | otherwise = case solver $ setElem p pos game
                  of Nothing -> guesser game pos ps
                     Just s -> Just s
