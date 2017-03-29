instance (Foldable f, Foldable g) => Foldable (Compose f g) where
   foldMap = foldMap . foldMap 

 
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
