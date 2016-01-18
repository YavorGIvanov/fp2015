module Utils 
( isValidIndex
, updateMatAt
, updateListAt
, getElemAt
, genFalseMat
, genFalseList
, indexTo1D
, indexTo2D
, updateRow
) where

import TypesAndParams


--Slower but easier
updateMatAt :: Index -> Bool -> BoolMat-> BoolMat
updateMatAt(i,j) newValue mat
	| i < 0 || i >= (length mat) || j < 0 || j >= (length (head mat)) = mat
 	| (upperRows, thisRow : lowerRows ) <- splitAt i mat
 	  ,(leftCells, _: rightCells) <- splitAt j thisRow = 
 	  	upperRows ++ (leftCells ++ (newValue : rightCells)) : lowerRows

updateRow :: Int -> BoolList -> BoolMat -> BoolMat
updateRow n newRow matrix
	| (upperRows, _:lowerRows) <- splitAt n matrix = upperRows++(newRow:lowerRows)
	| otherwise = error "Index out of range"

updateListAt :: Int  -> Bool -> BoolList -> BoolList
updateListAt i newValue vector
	| (leftCells, _:rightCells) <- splitAt i vector = leftCells++(newValue:rightCells) 
	| otherwise = error "Index out of range"

getElemAt :: Index -> BoolMat -> Bool
getElemAt (i, j) matrix = matrix !! i !! j 

genFalseMat :: Index -> BoolMat
genFalseMat (rowSize, colSize) = replicate rowSize (replicate colSize False)

genFalseList :: Int -> BoolList
genFalseList len = replicate len False

--Returns the nth column of a matrix		
takeNthCol :: Int -> BoolMat -> BoolList
takeNthCol n = map (head.drop n)

indexTo1D :: Index -> Int
indexTo1D (i, j)  = i * rows + j

indexTo2D :: Int -> Index
indexTo2D i = (i `div` rows, i `mod` cols)

isValidIndex :: Index -> Bool
isValidIndex (i, j) = i < rows && j < cols && j > -1 && i > -1