import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data FlipEither b a = Left' a | Right' b deriving (Show, Eq)

instance Functor (FlipEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a) = Left' (f a)

instance Applicative (FlipEither b) where
    pure x = Left' x
    Left' f <*> Left' x = Left' (f x)
    Right' b <*> _ = Right' b
    _ <*> Right' b = Right' b
    
instance Monad (FlipEither b) where
    return = pure
    Left' a >>= f = f a
    Right' b >>= _ = Right' b

instance (Arbitrary b, Arbitrary a) => Arbitrary (FlipEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Left' a, Right' b] 

instance (Eq b, Eq a) => EqProp (FlipEither b a) where (=-=) = eq
 
main :: IO ()
main = do 
    let trigger = undefined :: FlipEither String (Int,String,Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
