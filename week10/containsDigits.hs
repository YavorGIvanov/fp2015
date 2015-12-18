
containsDigits :: Int -> Int -> Bool
containsDigits x y = containsDigits' (show x) (show y)
	where 
		containsDigits' _ [] = True
		containsDigits' x (y:ys) 
			|elem y x = containsDigits' x ys
			|otherwise = False