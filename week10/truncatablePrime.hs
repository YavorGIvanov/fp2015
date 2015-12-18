isPrime :: Int -> Bool
isPrime x 
	|x < 2 = False
	|otherwise = isPrime' 2 
		where isPrime' divs 
			|divs*divs > x = True
			|mod x divs == 0 = False
			|otherwise = isPrime' $ divs + 1

trunctablePrime :: Int -> Bool
trunctablePrime 0 = True
trunctablePrime x 
	|isPrime x = trunctablePrime $ div x 10
	|otherwise = False 
