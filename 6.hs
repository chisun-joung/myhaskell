takeWhile1 :: ( a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x : xs)
	| p x = x : takeWhile1 p xs
	| otherwise = takeWhile1 p xs


takeWhile2 :: ( a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p (x : xs)
	| p x = x : takeWhile2 p xs
	| otherwise = []


takeWhile3 :: ( a -> Bool) -> [a] -> [a]
takeWhile3 _ [] = []
takeWhile3 p (x : xs)
	| p x =  takeWhile2 p xs
	| otherwise = []


takeWhile4 :: ( a -> Bool) -> [a] -> [a]
takeWhile4 p = foldl (\ acc x -> if p x then x : acc else acc ) []


dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x : xs)
	| p x = dropWhile1 p xs
	|otherwise = x : xs

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x : xs)
	| p x = dropWhile2 p xs
	|otherwise = xs


dropWhile3 :: (a -> Bool) -> [a] -> [a]
dropWhile3 p = foldr (\ x acc -> if p x then acc else x : acc ) []

dropWhile4 :: (a -> Bool) -> [a] -> [a]
dropWhile4 p = foldl add []
	where add [] x = if p x then [] else [x]
              add acc x = x : acc


map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\ x xs -> xs ++ [f x] ) []

--map2 :: (a -> b) -> [a] -> [b]
--map2 f = foldr (\ x xs -> f x ++ xs ) []

map3 :: (a -> b) -> [a] -> [b]
map3 f = foldl(\xs x -> f x : xs) []


map4 :: (a -> b) -> [a] -> [b]
map4 f = foldl(\xs x -> xs ++ [f x]) []


--compose :: [a -> a] -> (a ->a)
--compose = foldr (.) id

--sumsqreven = compose [sum, map (^ 2), filter even]
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)


type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin_unfold :: Int -> [Bit]
int2bin_unfold = unfold (== 0) (`mod` 2) (`div` 2)


chop8:: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)