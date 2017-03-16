> import Test.QuickCheck
> import Test.QuickCheck.Checkers
> import Test.QuickCheck.Classes

> data Pair a = Pair a a deriving Show

> instance Functor Pair where
>    fmap f (Pair x x') = Pair (f x) (f x')

> instance Applicative Pair where
>   pure x = Pair x x
>   (<*>) (Pair f f') (Pair x x') = Pair (f x) (f' x')

> instance Arbitrary a => Arbitrary (Pair a) where
>   arbitrary = do
>        x <- arbitrary
>        y <- arbitrary
>        return (Pair x y)

 instance Eq a => EqProp (Pair a) where
   (=-=) (Pair x x') (Pair y y') = x `eq` x' && y `eq` y'

> data Two a b = Two a b

> instance Functor (Two a) where
>   fmap f (Two a b) = Two a (f b)

> instance Monoid a => Applicative (Two a) where
>   pure x = Two mempty x
>   (<*>) (Two a f) (Two a' x) = Two (a `mappend` a') (f x)

> data Three a b c = Three a b c
> instance Functor (Three a b) where
>   fmap f (Three a b c) = Three a b (f c)

> instance (Monoid a, Monoid b) => Applicative (Three a b) where
>   pure x = Three mempty mempty x
>   (<*>) (Three a b f) (Three a' b' x) = Three (a `mappend` a') (b `mappend` b') (f x)

> data Three' a b = Three' a b b

> instance Functor (Three' a) where
>   fmap f (Three' a b b') = Three' a (f b) (f b')

> instance Monoid a => Applicative (Three' a) where
>   pure x = Three' mempty x x
>   (<*>) (Three' a f f') (Three' a' x x') = Three' (a `mappend` a') (f x) (f' x')

> data Four a b c d = Four a b c d

> instance Functor (Four a b c) where
>   fmap f (Four a b c x) = Four a b c (f x)

> instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
>   pure x = Four mempty mempty mempty x
>   (<*>) (Four a b c f) (Four a' b' c' x) =
>     Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (f x)

> data Four' a b = Four' a a a b

> instance Functor (Four' a) where
>   fmap f (Four' a a' a'' x) = Four' a a' a'' (f x)

> instance Monoid a => Applicative (Four' a) where
>   pure x = Four' mempty mempty mempty x
>   (<*>) (Four' a a' a'' f) (Four' b b' b'' x) =
>       Four' (a `mappend` b) (a' `mappend` b') (a'' `mappend` b'') (f x)
