module GaussianElimination 
(gaussianElimination) 
	where

import TypesAndParams
import Utils
import Data.Bits(xor)

testBoard :: BoolMat
testBoard =  [[True, True, False, False, True],
			  [False, False, False, False, False],
			  [False, True, True, True, False],
			  [False, True, True, True, True],
			  [True, False, True, False, False]]

--Returns the index of the pivot row
findPivotRow :: Int -> BoolMat -> Int
findPivotRow currentCol = length.takeWhile (==False).(takeNthCol currentCol)

addTwoRows :: Int -> Int -> BoolMat -> BoolMat
addTwoRows pivotRow currentRow matrix = updateRow currentRow (zipWith xor (matrix !! pivotRow) (matrix !! currentRow)) matrix

gaussianElimination :: BoolMat -> BoolMat  
gaussianElimination matrix 





