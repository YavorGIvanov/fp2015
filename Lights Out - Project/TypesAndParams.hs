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
rows = 5

cols :: Int
cols = 5

maxRows :: Int
maxRows = rows*rows

maxCols :: Int
maxCols = cols*cols
