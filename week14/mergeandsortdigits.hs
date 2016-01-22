import Data.List

sortAsc :: [Int] -> [Int]
sortAsc = sort

sortDesc :: [Int] -> [Int]
sortDesc = reverse . sort

intToList :: Int -> [Int]
intToList 0 = []
intToList x = (intToList (div x 10)) ++ [(mod x 10)]

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt xs = read (concat $ map show xs) :: Int

sumDigits :: Int -> Int
sumDigits x = sum $ intToList x

mergeAndSortDigits :: Int -> Int -> Int
mergeAndSortDigits x y
	|sumDigits x <= sumDigits y = listToInt . (map head) . group . sortAsc $ ls
	|otherwise  = listToInt . (map head) . group . sortDesc $ ls
		where ls = filter (/=0) (intToList x ++ intToList y)