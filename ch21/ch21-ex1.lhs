> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

> newtype Identity a = Identity a deriving (Eq, Ord, Show)

> instance Functor Identity where
>   fmap f (Identity x) = Identity (f x)

> instance Foldable Identity where
>   foldr f z (Identity x) = f x z

> instance Applicative Identity where
>   pure x = Identity x
>   Identity f <*> Identity x = Identity $ f x

> instance Traversable Identity where
>   traverse f (Identity x) = Identity <$> f x

> instance Arbitrary a => Arbitrary (Identity a) where
>   arbitrary = do
>       a <- arbitrary
>       return (Identity a)

> instance Eq a => EqProp (Identity a) where
>   (=-=) = eq

> newtype Constant a b = 
>   Constant { getConstant :: a } deriving (Eq, Show, Ord)

> instance Functor (Constant a) where
>   fmap _ (Constant a) = Constant a

> instance Foldable (Constant a) where
>   foldr _ z (Constant a) = z

> instance Monoid a => Applicative (Constant a) where
>   pure x = Constant mempty
>   (Constant a) <*> (Constant a') = Constant (a `mappend` a')

> instance Traversable (Constant a) where
>   traverse _ (Constant a) = pure (Constant a) 

> instance Arbitrary a => Arbitrary (Constant a b) where
>   arbitrary = do
>       a <- arbitrary
>       return (Constant a)

> instance Eq a => EqProp (Constant a b) where
>   (=-=) = eq

> data Optional a = Nada | Yep a deriving (Show, Eq)

> instance Functor Optional where
>   fmap _ Nada = Nada
>   fmap f (Yep x) = Yep (f x)

> instance Applicative Optional where
>   pure x = Yep x
>   Nada <*> _ = Nada
>   _ <*> Nada = Nada
>   Yep f <*> Yep x = Yep (f x)

> instance Foldable Optional where
>   foldr _ z Nada = z
>   foldr f z (Yep x) = f x z

> instance Traversable Optional where
>   traverse _ Nada = pure Nada
>   traverse f (Yep x) = Yep <$> f x

> instance Arbitrary a => Arbitrary (Optional a) where
>   arbitrary = do
>       a <- arbitrary
>       elements [Nada, Yep a]

> instance Eq a => EqProp (Optional a) where
>   (=-=) = eq

> data List a = Nil | Cons a (List a) deriving (Eq, Show, Ord)

> instance Functor List where
>   fmap _ Nil = Nil
>   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

> instance Monoid (List a) where
>   mempty = Nil
>   Nil `mappend` x = x
>   x `mappend` Nil = Nil
>   Cons x xs `mappend` ys = Cons x (xs `mappend` ys)

> instance Applicative List where
>   pure x = Cons x Nil
>   Nil <*> _ = Nil
>   _ <*> Nil = Nil
>   Cons f fs <*> xs = (fmap f xs) `mappend` (fs <*> xs)

> instance Foldable List where
>   foldr _ z Nil = z
>   foldr f z (Cons x xs) = f x (foldr f z xs)

> instance Traversable List where
>   traverse _ Nil = pure Nil
>   traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

> instance Arbitrary a => Arbitrary (List a) where
>   arbitrary = do
>       a <- arbitrary
>       b <- arbitrary
>       elements [Nil, Cons a Nil, Cons a (Cons b Nil)]

> instance Eq a => EqProp (List a) where
>   Nil =-= Nil = property True
>   Cons x xs =-= Cons y ys = x == y .&. xs =-= ys
>   _ =-= _ = property False

> data Three a b c = Three a b c deriving (Show, Eq, Ord)

> instance Functor (Three a b) where
>   fmap f (Three a b x) = Three a b (f x)

> instance (Monoid a, Monoid b) => Applicative (Three a b) where
>   pure x = Three mempty mempty x
>   Three a b f <*> Three a' b' x = 
>       Three (a `mappend` a') (b `mappend` b') (f x)

> instance Foldable (Three a b) where
>   foldr f z (Three _ _ x) = f x z

> instance Traversable (Three a b) where
>   traverse f (Three a b x) = Three a b <$> f x

> instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
>    Arbitrary (Three a b c) where
>       arbitrary = do
>           a <- arbitrary
>           b <- arbitrary
>           c <- arbitrary
>           return (Three a b c)

> instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where 
>   (=-=) = eq

> data Three' a b = Three' a b b deriving (Eq, Show)

> instance Functor (Three' a) where
>   fmap f (Three' a x x') = Three' a (f x) (f x')

> instance Monoid a => Applicative (Three' a) where
>   pure x = Three' mempty x x
>   Three' a f f' <*> Three' a' x x' =
>       Three' (a `mappend` a') (f x) (f' x')

> instance Foldable (Three' a) where
>   foldr f z (Three' a x x') = f x (f x' z)

the definition
    foldr f z (Three' a x x') = f x' (f x z) 
was incorrect because the mappend operation came out backwards: 
    foldMap id (Three' undefined "a" "b") == "ba"
should be == "ab"

    can also implement like this: 

   foldMap f (Three' a x x') = f x `mappend` f x'

> instance Traversable (Three' a) where
>   traverse f (Three' a x x') = Three' a <$> f x <*> f x'

> instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
>   arbitrary = do
>           a <- arbitrary
>           b <- arbitrary
>           c <- arbitrary
>           return (Three' a b c)

> instance (Eq a, Eq b) => EqProp (Three' a b) where
>   (=-=) = eq

> main = do
>    let trigger = undefined :: Identity (Int, Int, [Int])
>    quickBatch (traversable trigger)
>    let t2 = undefined :: Constant String (Int, Int, [Int])
>    quickBatch (traversable t2)
>    let t3 = undefined :: Optional (Int, Int, [Int])
>    quickBatch (traversable t3)
>    let t4 = undefined :: List (Int, Int, [Int])
>    quickBatch (traversable t4)
>    let t5 = undefined :: Three String [Int] (Int, Int, [Int])
>    quickBatch (traversable t5)
>    let t6 = undefined :: Three' Int (Int, Int, [Int])
>    verboseBatch (traversable t6)

