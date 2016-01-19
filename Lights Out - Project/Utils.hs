module Utils 
( updateMatAt
, updateRow
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

--Slower but easier
updateMatAt :: Index -> Bool -> BoolMat-> BoolMat
updateMatAt (i,j) newValue mat
	| i < 0 || i >= (length mat) || j < 0 || j >= (length (head mat)) = mat
 	| (upperRows, thisRow : lowerRows ) <- splitAt i mat
 	, (leftCells, _: rightCells) <- splitAt j thisRow = upperRows ++ (leftCells ++ (newValue : rightCells)) : lowerRows

updateRow :: Int -> BoolList -> BoolMat -> BoolMat
updateRow n newRow matrix
	| (upperRows, _:lowerRows) <- splitAt n matrix = upperRows++(newRow:lowerRows)
	| otherwise = error "Index out of range"

swapRows :: Int -> Int -> BoolMat -> BoolMat
swapRows i j matrix 
	| i < 0 || i >= maxRows || j < 0 || j  >= maxRows = error ("Indices of rows out of range " ++ (show i) ++ " " ++ (show j))
	| i == j = matrix
	| i > j = swapRows j i matrix
	| (upperRows, iRow:bottomRows) <- splitAt i matrix
    , (middleRows, jRow:botRows) <- splitAt (j - (length upperRows + 1)) bottomRows = upperRows ++ (jRow:middleRows) ++ (iRow:botRows)
    | otherwise = error ("blaaaa " ++ (show i) ++ " " ++ (show j))

updateListAt :: Int  -> Bool -> BoolList -> BoolList
updateListAt i newValue vector
	| not $ isValid1DIndex i vector = error "Index out of range"
	| (leftCells, _:rightCells) <- splitAt i vector = leftCells++(newValue:rightCells) 

getElemAt :: Index -> BoolMat -> Bool
getElemAt (i, j) matrix = matrix !! i !! j 

genFalseMat :: Index -> BoolMat
genFalseMat (rowSize, colSize) = replicate rowSize (replicate colSize False)

genFalseList :: Int -> BoolList
genFalseList len = replicate len False

addColumnBack :: BoolList -> BoolMat -> BoolMat
addColumnBack xs mat = transpose (transpose mat ++ [xs])

--Returns the nth column of a matrix		
takeNthCol :: Int -> BoolMat -> BoolList
takeNthCol n = map (head.drop n)

removeLastColumn :: BoolMat -> BoolMat
removeLastColumn = map init 

index1DTo2D :: Int -> Index
index1DTo2D i = (i `div` maxRows, i `mod` maxCols)

isItEchelon :: BoolMat -> Bool
isItEchelon = (\z -> length z == 0).filter (\(x,y) -> (snd (index1DTo2D x)) < (fst (index1DTo2D x)) && y == True).zip [0..].concat

isValid1DIndex :: Int -> BoolList -> Bool
isValid1DIndex i xs = i > -1 && i < length xs