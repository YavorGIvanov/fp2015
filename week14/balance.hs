import Data.List

balance :: Int -> [Int] -> Int
balance _ [] = 0
balance suml xs = balance' suml 0 (reverse $ sort  xs)
	where balance' suml count xs
		|xs == [] || sum xs <= suml = count
		|otherwise= balance' suml (count+1) $ tail xs