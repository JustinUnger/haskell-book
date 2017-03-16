> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

> data S n a = S (n a) a deriving (Eq, Show, Ord)

> foo = S Nothing "foo"
> bar = S (Just "foo") "bar"

> instance Functor n => Functor (S n) where
>   fmap f (S m x) = S (fmap f m) (f x)

> instance Foldable n => Foldable (S n) where
>  foldr f z (S m x) = foldr f (f x z) m

> instance Traversable n => Traversable (S n) where
>   traverse f (S m x) = S <$> (traverse f m) <*> f x

> instance (Arbitrary (n a), Arbitrary a, CoArbitrary a) => Arbitrary (S n a) where
>   arbitrary = do
>       n <- arbitrary
>       a <- arbitrary
>       return (S (n a) a)

> instance (Eq (n a), Eq a) => EqProp (S n a) where
>   (=-=) = eq

> main = do
>   let trig = undefined :: S Maybe (Int, Int, [Int])
>   let trig2 = undefined :: S [] (Int, Int, [Int])
>   quickBatch (traversable trig2)
