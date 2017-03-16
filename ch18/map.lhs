monadicAction :: Monad m => a -> m b
monadicAction = undefined

even' :: Integral a => a -> Maybe a
even' x = if even x then Just x else Nothing


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr (\_ acc -> acc) (return []) xs

foo [] _ = return []
foo (x:xs) f = 
    (f x) >>= (\b -> return [b])
 

traverse f = foldr (cons_f f) (pure [])

cons_f f x ys = (:) <$> f x <*> ys

bar :: Monad m => [a] -> (a -> m b) -> m [b]
bar [] _ = return []
--  bar (x:xs) f = fmap (:) (f x) <*> bar xs f
bar (x:xs) f = (:) <$> f x <*> bar xs f


ap mf mx = do
    f <- mf
    x <- mx
    return (f x)

ap' mf mx =
    mf >>= 
        \f -> mx >>=
            \x -> return (f x)

mf ~ Maybe ([a] -> [a])
mx ~ Maybe [a]

    Maybe ([a] -> [a]) >>= \ ([a] -> [a]) -> 
        Maybe [a] >>= \ [a] -> return (([a] -> [a]) [a])


    


