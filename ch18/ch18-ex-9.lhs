> meh :: Monad m => [a] -> (a -> m b) -> m [b]
> meh [] _ = return []
> meh (x:xs) f = fmap (:) (f x) <*> meh xs f
