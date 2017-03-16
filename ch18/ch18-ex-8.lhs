
> a :: Monad m => m a -> m (a -> b) -> m b
> a ma mf = mf >>= (\f -> ma >>= (\x -> return (f x)))

> a' :: Monad m => m a -> m (a -> b) -> m b
> a' ma mf = do
>   f <- mf
>   x <- ma
>   return (f x)

