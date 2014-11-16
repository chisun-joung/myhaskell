and1 :: [Bool] -> Bool
and1 [] = True
and1 (b : bs) 
	| b = b
	| otherwise = and1 bs


concat1 :: [[a]] -> [a]
concat1 [[]] = []
concat1 (xs : xss) = xs ++ concat xss

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n - 1) x


elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 x (y : ys)
	| x == y = True
	| otherwise = elem1 x ys