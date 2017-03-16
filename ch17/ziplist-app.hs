data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' $ pure x
    (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $ zipWith' ($) xs ys

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ _ Nil = Nil
zipWith' _ Nil _ = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

repeat' x = Cons x (repeat' x)
