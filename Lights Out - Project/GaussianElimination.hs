module GaussianElimination 
(gaussianElimination) 
	where

import TypesAndParams
import Utils
import Data.Bits(xor)
import Data.List
import ToggleMatrix

test :: BoolMat
test = addColumnBack (concat testBoard) toggleMat

testBoard :: BoolMat
testBoard =  [[True, False, True, False, False],
			  [True, False, True, False, True],
			  [False, False, False, True, True],
			  [False, False, True, True, True],
			  [False, False, True, True, False]]

--Returns the index of the next pivot row
findPivot :: Int -> Int -> BoolMat -> Int
findPivot startRow currCol = (\x -> x + startRow).length.takeWhile (==False).drop startRow.takeNthCol currCol

--Sums up two rows using xor
sumTwoRows :: BoolList -> BoolList -> BoolList
sumTwoRows pivotList ys
	| not (ys !! (length (takeWhile (==False) pivotList))) = ys
	| otherwise = zipWith xor pivotList ys

makeEchelon :: Int -> BoolMat -> BoolMat
makeEchelon numCol matrix
	| pivotIndex < length matrix && pivotIndex > -1 = upper ++ (pivot:(map (\x -> sumTwoRows pivot x) bottom))
	| otherwise = matrix
		where 
			(upper, pivot : bottom) = splitAt numCol (swapRows numCol pivotIndex matrix)
			pivotIndex = (findPivot numCol numCol matrix)

gaussianElimination :: BoolMat -> BoolMat
gaussianElimination matrix = gaussianElmination' 0 matrix
	where gaussianElmination' numCol mat
		| numCol == (maxCols - 1) = mat
		| otherwise = gaussianElmination' (numCol + 1) (makeEchelon numCol mat)







