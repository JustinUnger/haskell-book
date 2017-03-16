import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Monad (join)

data List a = Nil | Cons a (List a) deriving (Show)

instance Monoid (List a) where
    mempty = Nil
    Nil `mappend` x = x
    x `mappend` Nil = x
    Cons x xs `mappend` ys = Cons x (xs `mappend` ys)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil 
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = fmap f xs `mappend` (fs <*> xs)

concat' :: List (List a) -> List a
concat' Nil = Nil
concat' (Cons x xs) = mappend x (concat' xs)
    
instance Monad List where
    return = pure
    Nil >>= _ = Nil
    Cons x xs >>= f = concat' $ fmap f (Cons x xs)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Cons a Nil, Cons a (Cons b Nil), Nil]

instance EqProp a => EqProp (List a) where 
    Nil =-= Nil = property True
    Cons x xs =-= Cons y ys = x =-= y .&. xs =-= ys
    _ =-= _ = property False
 
main :: IO ()
main = do 
    let trigger = undefined :: List (Int,String,Int)
    quickBatch $ monoid trigger
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
