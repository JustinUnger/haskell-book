import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = fmap f xs `append` (<*>) fs xs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x  <- arbitrary
        x' <- arbitrary
        elements [Nil, Cons x Nil, Cons x (Cons x' Nil)]

instance EqProp a => EqProp (List a) where
    Nil =-= Nil = property True
    Cons x xs =-= Cons y ys = x =-= y .&. xs =-= ys
    _ =-= _ = property False

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

toMyList :: [a] -> List a
toMyList [] = Nil
toMyList (x:xs) = Cons x (toMyList xs)

fromMyList :: List a -> [a]
fromMyList Nil = []
fromMyList (Cons x xs) = x : (fromMyList xs)

main = do
    let trigger = undefined :: List (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
