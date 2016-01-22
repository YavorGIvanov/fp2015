{-|
	This module implements the Gaussian Elimination algorithm -> https://en.wikipedia.org/wiki/Gaussian_elimination
	Pivot -> next row containing True value
	Echelon -> only zeros in a matrix below the main diagonal
-}
module GaussianElimination 
(gaussianElimination) 
	where

import TypesAndParams
import Utils
import Data.Bits(xor)
import Data.List
import ToggleMatrix


-- |Returns the index of the next pivot row(row which contains True)
findPivot :: Int -> Int -> BoolMat -> Int
findPivot startRow currCol = (\x -> x + startRow).length.takeWhile (==False).drop startRow.takeNthCol currCol

-- |sums up two rows using xor and returns the resulting list. Only sums if
-- |the value at pivotPosition in ys is True. 
-- |pivotPosition is the position in the pivotRow where the first True value is found
sumTwoRows :: BoolList -> BoolList -> BoolList
sumTwoRows pivotList ys
	| not (ys !! (length (takeWhile (==False) pivotList))) = ys
	| otherwise = zipWith xor pivotList ys

-- |transforms a particular column in echelon form
makeEchelon :: Int -> BoolMat -> BoolMat
makeEchelon numCol matrix
	| pivotIndex < length matrix && pivotIndex > -1 = upper ++ (pivotRow:(map (\x -> sumTwoRows pivotRow x) bottom))
	| otherwise = matrix
		where 
			(upper, pivotRow : bottom) = splitAt numCol (swapRows numCol pivotIndex matrix)
			pivotIndex = (findPivot numCol numCol matrix)

-- |transforms a boolean matrix in echelon form
gaussianElimination :: BoolMat -> BoolMat
gaussianElimination matrix = gaussianElmination' 0 matrix
	where gaussianElmination' numCol mat
		| numCol == (maxRows-1) = mat
		| otherwise = gaussianElmination' (numCol + 1) (makeEchelon numCol mat)
			






