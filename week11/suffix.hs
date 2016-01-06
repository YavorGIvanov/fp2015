suffix :: (Eq a) => [a] -> [a] -> Bool
suffix xs ys = xs == drop (length ys - length xs) ys