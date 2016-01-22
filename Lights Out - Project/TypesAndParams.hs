{-|
	In this module we store the main types and parameters used in the whole project
-}
module TypesAndParams 
( BoolList
, BoolMat
, Index
, rows
, cols
, maxRows
, maxCols
) where

type BoolList = [Bool]
type BoolMat = [BoolList]
type Index = (Int, Int)

rows :: Int
rows = 9

cols :: Int
cols = 9

maxRows :: Int
maxRows = rows*rows

maxCols :: Int
maxCols = cols*cols
