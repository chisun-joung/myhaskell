import Data.Char
import Control.Monad

type Parser a = String -> [(a, String)]


return1 :: a -> Parser a
return1 v = \inp -> [(v,inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of 
					[] -> []
					(x: xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp


p :: Parser(Char, Char)
p = do 
	x <- item
	item
	y <- item
	return (x,y)