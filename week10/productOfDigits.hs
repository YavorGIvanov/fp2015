productOfDigits :: Int -> Int
productOfDigits x
	|x < 10 = x
	|otherwise = (mod x 10) * (productOfDigits $ div x 10)