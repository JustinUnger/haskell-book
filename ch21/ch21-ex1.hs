newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
   foldr f z (Identity x) = f x z

instance Applicative Identity where
   pure x = Identity x
   Identity f <*> Identity x = Identity $ f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x
