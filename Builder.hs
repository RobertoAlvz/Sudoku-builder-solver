import System.Random
import Data.Matrix
import Data.List
import Basic

main = do
    r <- randomRIO (0,20) :: IO Int
    p <- randomRIO (0,80) :: IO Int
    putStrLn "Dificultad [1 .. 80]: "
    n <- getLine 
    putStrLn . prettyMatrix $ hider (buildIO r) (read n) p 11

buildIO :: Int -> Sudoku
buildIO = just . build (matrix 9 9 $ \_ -> 0)

build :: Sudoku -> Int -> Maybe Sudoku
build s r
    | finished s = Just s
    | l == 0 || (null a) = Nothing
    | otherwise = builder s p pos r 
    where a = availables s
          p = (!!) a $ mod r $ length a
          pos = shuffle (posibles s p) r
          l = length pos

builder :: Sudoku -> (Int, Int) -> [Int] -> Int -> Maybe Sudoku
builder _ _ [] _ = Nothing
builder s p (po:ps) r
    | aux == Nothing = builder s p ps r
    | otherwise = aux
    where aux = build (setElem po p s) r

hider :: Sudoku -> Int -> Int -> Int -> Sudoku
hider s 0 _ _ = s
hider s n p i
    | s ! pc == 0 = hider s n (mod (p+1) 81) i
    | otherwise = hider (setElem 0 pc s) (n-1) (mod (p+i) 81) i
    where pc = ((+1) $ div p 9, (+1) $ mod p 9)

just :: Maybe Sudoku -> Sudoku
just Nothing = matrix 9 9 $ \_ -> 0
just (Just s) = s

shuffle :: [a] -> Int -> [a]
shuffle l r = (!!) p $ mod r $ length p
    where p = permutations l 
