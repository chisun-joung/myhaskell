data Expr = Val Int | Div Expr Expr
--data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then Nothing else Just (n `div` m)

seqn :: Maybe a -> Maybe b -> Maybe (a, b)
seqn Nothing _ = Nothing
seqn _ Nothing = Nothing
seqn (Just x) (Just y) = Just (x,y) 


apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing = Nothing
apply f (Just x) = f x

--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--Nothing >>= _  = Nothing
--Just x >>= f = f x

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do 
					n <- eval x
					m <- eval y 
					safediv n m








































