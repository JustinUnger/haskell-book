> l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c

not sure if this qualifie as it uses <*> from Applicative

> -- l2 f ma mb = f <$> ma <*> mb

> l2 f ma mb = 
>   let fs = f <$> ma
>   in fs >>= (\g -> fmap g mb) 

> l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
> -- l2' f ma mb = f <$> ma >>= (\g -> fmap g mb) 
> l2' f ma mb = f <$> ma >>= (<$> mb)

> l2'' f ma mb =
>   ma >>= (\a -> mb >>= (\b -> return (f a b)))

> l2''' f ma mb =
>   ma >>=
>       \a -> mb >>=
>           \b -> return (f a b)

> liftA2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
> liftA2 f ma mb = do
>   a <- ma
>   b <- mb
>   return (f a b)

