module Basic
(Sudoku(..), availables, posibles, impos, finished, valid, toList)
where

import Utils
import Data.List
import Data.Matrix
import qualified Data.Vector as V

type Sudoku = Matrix Int

availables :: Sudoku -> [(Int,Int)]
availables = map (\ x -> ((+1) $ div x 9, (+1) $ mod x 9)) . elemIndices 0 . toList

posibles :: Sudoku -> (Int, Int) -> [Int]
posibles s p = if s ! p /= 0 then [s ! p] 
               else getPos s p 1

getPos :: Sudoku -> (Int, Int) -> Int -> [Int]
getPos s p x = if x == 9 then aux x
               else (aux x) ++ (getPos s p $ x+1)
               where aux = \ a -> if valid $ setElem a p s then [a] 
                                  else []

impos :: Sudoku -> (Int, Int) -> Bool
impos = (null .) . posibles

finished :: Sudoku -> Bool
finished = offv (&&) (notElem 0 . toList) valid
             
valid :: Sudoku -> Bool
valid = offv (&&) (offv (&&) (validV 9) (validH 9)) (validS (7,7))

validV :: Int -> Sudoku -> Bool
validV i s = if i == 1 then aux i s else (aux i s) && validV (i-1) s
    where aux = ((nonRep . V.toList) .) . getRow

validH :: Int -> Sudoku -> Bool
validH i s = if i == 1 then aux i s else (aux i s) && validH (i-1) s
    where aux = ((nonRep . V.toList) .) . getCol 

validS :: (Int,Int) -> Sudoku -> Bool
validS (i,j) s
    | i==1 && j==1 = aux 1 3 1 3 s
    | j==1 = (aux i (i+2) j (j+2) s) && validS (i-3,7) s
    | otherwise = (aux i (i+2) j (j+2) s) && validS (i,j-3) s
    where aux = (((((nonRep . toList) .) .) .) .) . submatrix

toList :: Matrix a -> [a]
toList = ofv toListInd nrows

toListInd :: Matrix a -> Int -> [a]
toListInd _ 0 = []
toListInd m i = (toListInd m $ i-1) ++ (V.toList $ getRow i m)

nonRep :: [Int] -> Bool
nonRep [] = True
nonRep (x:xs) = if (elem x xs) && x/=0 then False else nonRep xs
