removeAt :: Int -> [a] -> [a]
removeAt i xs
	|i < 0 || i >= length xs  = error "Invalid index"
	|otherwise = take i xs ++ drop (i+1) xs