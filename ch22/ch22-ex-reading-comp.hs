-- this is needed for Applicative exercise:

{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

-- write liftA2 yourself: 

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- write the following function: 

asks :: (r -> a) -> Reader r a
asks = Reader 

-- Implement the Applicative for Reader: 

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

-- or --

-- fmap f (Reader ra) = Reader $ (f . ra)

-- or --

-- fmap f (Reader ra) = Reader $ pure f <*> ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader (\_ -> a)
 
    (<*>) :: Reader r (a -> b)
          -> Reader r a
          -> Reader r b

    (Reader rab) <*> (Reader ra) =
        Reader $ \r -> rab r (ra r) 

-- compare fmap
--      Reader $ \r -> f (ra r)


