import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => 
                        (a -> b)
                    ->  (b -> c)
                    ->  f a 
                    ->  Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

data Pair a = Pair a a deriving (Eq, Show)

-- neither value in Pair are part of the functorial structure, as kind of 
-- Pair is already * -> *

instance Functor (Pair) where
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three' x y z)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
            Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d) 

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four' a b c d)

main = do
    quickCheck $ \x -> functorIdentity (x :: [Int])
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])

    quickCheck $ \x -> functorIdentity (x :: Identity Int)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)

    quickCheck $ \x -> functorIdentity (x :: Pair Int)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)
   
    quickCheck $ \x -> functorIdentity (x :: Two String Int)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two String Int)

    quickCheck $ \x -> functorIdentity (x :: Three String Int Double)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three String Int Double)
    
    quickCheck $ \x -> functorIdentity (x :: Three' String Int)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' String Int)

    quickCheck $ \x -> functorIdentity (x :: Four String Double Ordering Int)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four String Double Ordering Int) 

    quickCheck $ \x -> functorIdentity (x :: Four' String Int)
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four' String Int)
