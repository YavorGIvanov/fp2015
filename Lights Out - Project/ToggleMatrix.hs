import Utils
import TypesAndParams

genToggleMatFor :: Index -> BoolMat
genToggleMatFor index = genToggleMatFor' index $ genFalseMat (rows, cols)
	where 
		genToggleMatFor' (i,j) = updateMatAt (i+1, j) True . updateMatAt (i-1, j) True . 
								 updateMatAt (i, j+1) True  . updateMatAt (i, j-1) True . 
								 updateMatAt (i, j) True

genToggleList :: Index -> BoolList
genToggleList index = concat $ genToggleMatFor index

genToggleMat :: BoolMat
genToggleMat = genToggleMatFor ([0..rows], [0..cols])