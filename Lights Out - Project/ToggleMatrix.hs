module ToggleMatrix 
(toggleMat) 
	where

import Utils
import TypesAndParams
import Data.List

--Generates a 5x5 toggle matrix for a given index
genToggleMatFor :: Index -> BoolMat
genToggleMatFor index = genToggleMatFor' index $ genFalseMat (rows, cols)
	where 
		genToggleMatFor' (i,j) = updateMatAt (i+1, j) True . updateMatAt (i-1, j) True . 
								 updateMatAt (i, j+1) True  . updateMatAt (i, j-1) True . 
								 updateMatAt (i, j) True

--two list [1,2] ['a','b'] -> [(1,'a'),(2,'b')]
cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

--Flattens the 5x5 toggle matrix to a list with len 25
genToggleList :: Index -> BoolList
genToggleList index = concat $ genToggleMatFor index

--Generates 25x25 matrix with columns -> toggle matrices
toggleMat :: BoolMat
toggleMat = transpose $ map genToggleList $ cartesianProduct [0..(rows-1)] [0..(cols-1)]
