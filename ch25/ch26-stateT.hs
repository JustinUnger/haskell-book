
newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \s -> 
        fmap (\(a,s') -> (f a, s')) $ runStateT m s

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    (StateT smf) <*> (StateT sma) = StateT $ \s -> do
        (f,s') <- smf s
        (a,s'') <- sma s'
        return (f a, s'') 

instance (Monad m) => Monad (StateT s m) where
    return = pure
    
    (StateT sma) >>= f = StateT $ \s -> do
        (a,s') <- sma s
        runStateT (f a) s'
