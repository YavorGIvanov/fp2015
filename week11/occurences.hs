occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences _ [] = []
occurrences (x:xs) ys = (length . filter (==x) $ ys) : occurrences xs ys
