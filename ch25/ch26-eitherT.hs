newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT v) >>= f = EitherT $ do
        ema <- v
        case ema of
            Left  e -> return $ Left e
            Right a -> runEitherT (f a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema
swapEitherT' ema = EitherT $ fmap swapEither (runEitherT ema)

swapEitherT'' :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT'' = EitherT . fmap swapEither . runEitherT

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb) = do
    v <- amb
    case v of
        Left  a -> f a
        Right b -> g b
