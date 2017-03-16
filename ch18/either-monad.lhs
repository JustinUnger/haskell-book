> import Test.QuickCheck
> import Test.QuickCheck.Checkers 
> import Test.QuickCheck.Classes 

> data Sum a b = First a | Second b deriving (Eq, Show)

> instance Functor (Sum a) where
>   fmap _ (First a) = First a
>   fmap f (Second b) = Second (f b)

> instance Applicative (Sum a) where
>   pure = Second
>   (Second f) <*> (Second x) = Second (f x)
>   (First a) <*> _ = First a
>   _ <*> (First a) = First a

> instance Monad (Sum a) where
>   return = pure
>   (>>=) (First a) _ = First a
>   (>>=) (Second b) f = f b

try to test this with quickcheck

> instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
>   arbitrary = do
>       a <- arbitrary
>       b <- arbitrary
>       elements [First a, Second b]

> instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

> main :: IO ()
> main = do
>   let trigger = undefined :: Sum String (Int, String, Int)
>   quickBatch $ functor trigger
>   quickBatch $ applicative trigger
>   quickBatch $ monad trigger

