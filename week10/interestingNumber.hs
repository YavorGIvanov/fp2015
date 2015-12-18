sumOfDivisors :: Int -> Int
sumOfDivisors x = sumOfDivisors' 2
	where sumOfDivisors' divs
		|divs > (div x 2) = 1
		|mod x divs == 0 = divs + (sumOfDivisors' $ divs + 1)
		|otherwise = sumOfDivisors' $ divs + 1

interestingNumber :: Int -> Bool
interestingNumber x = x == (sumOfDivisors $ sumOfDivisors x)
