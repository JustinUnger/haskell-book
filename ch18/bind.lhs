> import Control.Monad (join)

> bind :: Monad m => (a -> m b) -> m a -> m b
> bind f m = join $ fmap f m

> f :: Monad m => (a -> m b)
> f = undefined

> m :: Monad m => m a
> m = undefined

