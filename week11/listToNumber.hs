listToNumber :: [Int] -> Int
listToNumber xs = (read $ concat $ map show xs) :: Int