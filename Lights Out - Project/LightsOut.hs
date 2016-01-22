import TypesAndParams
import Utils
import ToggleMatrix
import GaussianElimination
import BackSubstitution

{-- Lights Out 5x5 Solver (easily to change to nxn) --}

solve :: BoolMat -> BoolMat
solve mat = listToMatrix.backSubstitution.gaussianElimination $ solvable
	where solvable = addColumnBack (concat mat) toggleMat

listToMatrix :: BoolList -> BoolMat
listToMatrix [] = []
listToMatrix xs = (take rows xs) : listToMatrix (drop rows xs) 

--Bla

main = do 
	putStrLn "Welcome to the Lights Out 5x5 Solver!"
	putStrLn "Enter the puzzle you want me to solve: \n"
	puzzle <- getLine
	putStrLn "\nHere is the solution. True means click there. \nThe order of clicks doesn't matter."
	putStrLn "When you've clicked on all the positions \nwith value True the puzzle will be solved.\n"
	(mapM_ print).solve.read $ puzzle
	putStrLn ""

