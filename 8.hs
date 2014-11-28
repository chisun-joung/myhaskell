import System.IO


getCh :: IO Char
getCh = do 
			hSetEcho stdin False
			c <- getChar
			hSetEcho stdin True
			return c


hangman :: IO ()
hangman =
	do 
		putStrLn "Think of a word: "
	   	word <- sgetLine
	   	putStrLn "Try to guess it :"
	   	guess word


sgetLine :: IO String
sgetLine = do 
			x <- getCh
			if x == '\n' then
				do 
					putChar x
					return []
			else
				do 
					putChar '-'
					xs <- sgetLine
					return (x:xs)

guess :: String -> IO ()
guess word =
	do 
		putStr "> "
		xs <- getLine
		if xs == word then
			putStrLn "You got it!"
		else 
			do 
				putStrLn (diff word xs)
				guess word


diff :: String -> String -> String
diff xs ys = 
	[if elem x ys then x else '-' | x <- xs]
			


putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar  '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""


getLine' :: IO String
getLine' = get ""

get :: String -> IO String
get xs
	= do 
		x <- getChar
		case x of
			'\n' -> return xs
			_ -> get (xs ++ [x])

interact' :: (String -> String) -> IO()
interact' f
	= do 
		input <- getLine'
		putStrLn' (f input)


sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) =  m >> sequence_' ms

sequence' :: Monad m => [m a] -> m[a]
sequence' [] = return []
sequence' (m : ms)
	= do 
		a <- m
		as <- sequence' ms
		return (a : as)


mapM' :: Monad m => (a -> m b) ->[a] -> m [b]
mapM' f [] = return []
mapM' f (a : as) 
	= do 
		b <- f a
		bs <- mapM' f as
		return (b : bs)


filterM' :: Monad m => (a  -> m Bool) -> [a] -> m[a]
filterM' _ [] = return []
filterM' p (x : xs)
	= do 
		flag <- p x
		ys <- filterM' p xs
		if flag then return ys else return (x : ys)


foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a xs = foldr f' return xs a
	where f' x k z = f z x >>= k

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f a xs = foldl f' return xs a
	where f' k x z = f x z >>= k


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >> \ a -> return ( f a )

