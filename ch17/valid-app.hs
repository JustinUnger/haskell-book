import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Test.QuickCheck (Arbitrary, arbitrary, elements)

data Validation err a = 
    Failure err
  | Success a
  deriving (Eq, Show)

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Success a, Failure e]

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Success f) (Success x) = Success (f x)
    (<*>) (Failure x) (Failure x') = Failure (x `mappend` x')
    (<*>) (Failure x) _ = Failure x
    (<*>) _ (Failure x) = Failure x

trigger = undefined :: Validation [Int] (String, String, Int)

main = do
    verboseBatch $ applicative trigger

