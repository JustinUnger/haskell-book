> import Test.QuickCheck
> import Test.QuickCheck.Classes
> import Test.QuickCheck.Checkers

> data Tree a =
>     Empty
>   | Leaf a
>   | Node (Tree a) a (Tree a)
>   deriving (Eq, Show)

> instance Functor Tree where
>   fmap _ Empty = Empty
>   fmap f (Leaf x) = Leaf (f x)
>   fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

> instance Foldable Tree where
>   foldr _ z Empty = z
>   foldr f z (Leaf x) = f x z
>   foldr f z (Node l x r) = f x (foldr f (foldr f z r) l)

>   foldMap _ Empty = mempty
>   foldMap f (Leaf x) = f x 
>   foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

> instance Traversable Tree where
>   traverse _ Empty = pure Empty 
>   traverse f (Leaf x) = Leaf <$> (f x)
>   traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

> instance Arbitrary a => Arbitrary (Tree a) where
>   arbitrary = do
>       a <- arbitrary
>       b <- arbitrary
>       c <- arbitrary
>       elements [Empty, Leaf a, Node (Leaf a) b (Leaf c)]

> instance Eq a => EqProp (Tree a) where
>   Empty =-= Empty = property True
>   Leaf x =-= Leaf y = x `eq` y
>   Node l x r =-= Node l' x' r' = x `eq` x' .&. l =-= l' .&. r =-= r'
>   _ =-= _ = property False

> main = do
>   let trig = undefined :: Tree (Int, Int, [Int])
>   quickBatch (traversable trig)
