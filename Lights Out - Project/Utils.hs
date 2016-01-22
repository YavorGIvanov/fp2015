{-|
	In this module are stored all the utility functions used in the project.
	They are used for working with matrices and lists in an easier manner.
-}
module Utils 
( updateMatAt
, updateRow
, updateCol
, swapRows
, updateListAt
, getElemAt
, genFalseMat
, genFalseList
, takeNthCol
, addColumnBack
, removeLastColumn
, index1DTo2D
, isValid1DIndex
, isItEchelon
) where

import TypesAndParams
import Data.List

-- |changes(updates) a whole row in a matrix by a given number in the interval [0..length-1]
updateRow :: Int -> BoolList -> BoolMat -> BoolMat
updateRow n newRow matrix
	| (upperRows, _:lowerRows) <- splitAt n matrix = upperRows++(newRow:lowerRows)
	| otherwise = error "Index out of range"

-- |changes(updates) a whole column in a mtrix by a given number in the interval [0..(length (head matrix))-1]
updateCol :: Int -> BoolList -> BoolMat -> BoolMat
updateCol n newCol = transpose.updateRow n newCol.transpose

-- |swaps two rows in matrix by given indices and returns the resulting matrix
swapRows :: Int -> Int -> BoolMat -> BoolMat
swapRows i j matrix 
	| i < 0 || i >= maxRows || j < 0 || j  >= maxRows = error ("Indices of rows out of range " ++ (show i) ++ " " ++ (show j))
	| i == j = matrix
	| i > j = swapRows j i matrix
	| (upperRows, iRow:bottomRows) <- splitAt i matrix
    , (middleRows, jRow:botRows) <- splitAt (j - (length upperRows + 1)) bottomRows = upperRows ++ (jRow:middleRows) ++ (iRow:botRows)
    | otherwise = error ("blaaaa " ++ (show i) ++ " " ++ (show j))

-- |changes(updates) a specific element in a matrix by a given position and new value
updateMatAt :: Index -> Bool -> BoolMat-> BoolMat
updateMatAt (i,j) newValue mat
	| i < 0 || i >= (length mat) || j < 0 || j >= (length (head mat)) = mat
 	| (upperRows, thisRow : lowerRows ) <- splitAt i mat
 	, (leftCells, _: rightCells) <- splitAt j thisRow = upperRows ++ (leftCells ++ (newValue : rightCells)) : lowerRows

-- |changes(updates) a specific element in a list by a given position and new value
updateListAt :: Int  -> Bool -> BoolList -> BoolList
updateListAt i newValue vector
	| not $ isValid1DIndex i vector = error "Index out of range"
	| (leftCells, _:rightCells) <- splitAt i vector = leftCells++(newValue:rightCells) 

-- |returns the element in the passed matrix at position (i, j)
getElemAt :: Index -> BoolMat -> Bool
getElemAt (i, j) matrix = matrix !! i !! j 

-- |adds a column to the back of an already existing matrix
addColumnBack :: BoolList -> BoolMat -> BoolMat
addColumnBack xs mat = transpose (transpose mat ++ [xs])

-- |returns the nth column of a matrix		
takeNthCol :: Int -> BoolMat -> BoolList
takeNthCol n = map (head.drop n)

-- |removes the last column of a matrix and returns the resulting one
removeLastColumn :: BoolMat -> BoolMat
removeLastColumn = map init 

-- |given a size index, generates a rowSize x colSize False matrix 
genFalseMat :: Index -> BoolMat
genFalseMat (rowSize, colSize) = replicate rowSize (replicate colSize False)

-- |given a size, generates a False list with length -> len
genFalseList :: Int -> BoolList
genFalseList len = replicate len False

-- |transforms an index of a list to an index of an matrix
index1DTo2D :: Int -> Index
index1DTo2D i = (i `div` maxRows, i `mod` maxCols)

-- |checks if a matrix is in echelon form (echelon form -> https://en.wikipedia.org/wiki/Row_echelon_form)
isItEchelon :: BoolMat -> Bool
isItEchelon = (\z -> length z == 0).filter (\(x,y) -> (snd (index1DTo2D x)) < (fst (index1DTo2D x)) && y == True).zip [0..].concat

-- |checks if an index is within the bounds of a given list
isValid1DIndex :: Int -> BoolList -> Bool
isValid1DIndex i xs = i > -1 && i < length xs