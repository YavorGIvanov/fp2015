module TypesAndParams 
( BoolList
, BoolMat
, Index
, rows
, cols
) where

type BoolList = [Bool]
type BoolMat = [BoolList]
type Index = (Int, Int)

rows :: Int
rows = 5

cols :: Int
cols = 5
