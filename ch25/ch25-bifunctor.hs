class BiFunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b deriving Show

instance BiFunctor Deux where
    bimap f f' (Deux a b) = Deux (f a) (f' b)

data Const a b = Const a deriving Show

instance BiFunctor Const where
    bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c deriving Show

instance BiFunctor (Drei a) where
    bimap f f' (Drei a b c) = Drei a (f b) (f' c)

data SuperDrei a b c = SuperDrei a b deriving Show

instance BiFunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance BiFunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance BiFunctor (Quadriceps a b) where
    bimap f f' (Quadzzz a b c d) = Quadzzz a b (f c) (f' d)

data Either' a b = Left' a | Right' b

instance BiFunctor (Either') where
    bimap f _ (Left' a)  = Left' (f a)
    bimap _ f (Right' b) = Right' (f b)

