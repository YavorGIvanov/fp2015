{-|
	This module implements the Back Substitution algorithm -> https://en.wikipedia.org/wiki/Gaussian_elimination
	The process of solving a linear system of equations that has been transformed into row-echelon form or reduced row-echelon form. 
	The last equation is solved first, then the next-to-last, etc.
-}
module BackSubstitution 
(backSubstitution) 
	where

import TypesAndParams
import Utils
import GaussianElimination
import ToggleMatrix
import Data.Bits(xor)
import Data.List

findPivot :: BoolList -> Bool
findPivot xs
	| takeWhile (==False) xs == [] = False 

minLenList :: BoolList -> BoolList -> BoolList
minLenList xs ys = if (length (filter (==True) xs) < length (filter (==True) ys)) then xs else ys

backSubstitution :: BoolMat -> BoolList
backSubstitution matrix = backSubstitution' (maxRows - 1) []
	where 
		backSubstitution' (-1) found = found
		backSubstitution' numRow found 
			| not pivot && pivotValue = error "No solution"
			| not pivot && not pivotValue = minLenList (backSubstitution' (numRow-1) (True:found)) (backSubstitution' (numRow-1) (False:found))
			| otherwise = backSubstitution' (numRow-1) (pivotValue:found)
				where
					pivotValue = foldr xor lastElem (zipWith (&&) found (init right))
					(_, pivot:right) = splitAt numRow currRow 
					lastElem = last currRow
					currRow = matrix !! numRow  
			