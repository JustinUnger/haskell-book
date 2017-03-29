{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
    Applicative (Compose f g) where

    pure :: a -> Compose f g a
    --pure a = Compose (pure (pure a))
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    -- lift ap over composed functions, then ap result to composed values
  --(Compose f) <*> (Compose a) = Compose $ ((<*>) <$> f) <*> a
    (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a

-- test data
f :: Compose [] Maybe (Int -> Int)
f = Compose $ [Just (+ 1), Nothing]

x :: Compose [] Maybe Int
x = Compose $ [Nothing, Just 1]

-- unwrapped versions
f' :: [Maybe (Int -> Int)]
(Compose f') = f
x' :: [Maybe Int]
(Compose x') = x

f'' :: [Maybe (Int -> Int)]
f'' = getCompose f

x'' :: [Maybe Int]
x'' = getCompose x
    
c1 :: Compose [] Maybe Int
c1 = f <*> x

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
 -- foldMap :: Monoid m => (a -> m) -> Compose f g a -> m    
    foldMap f (Compose a) = (foldMap . foldMap) f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
 -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t dab)
 -- traverse :: (a -> f1 b) -> Compose f g a -> f1 (Compose f g b) 
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

