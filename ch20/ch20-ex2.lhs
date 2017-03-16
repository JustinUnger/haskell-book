write Foldable instances for the following datatypes: 

1. 

> data Constant a b = Constant a

> instance Foldable (Constant a) where
>   foldr _ z (Constant _) = z

2. 

> data Two a b = Two a b

> instance Foldable (Two a) where
>   foldr f z (Two _ x) = (f x z)

3. 

> data Three a b c = Three a b c

> instance Foldable (Three a b) where
>   foldr f z (Three _ _ x) = (f x z)

4,

> data Three' a b = Three' a b b

> instance Foldable (Three' a) where
>   foldr f z (Three' _ x x') = f x' . f x $ z

5. 

> data Four' a b = Four' a b b b

> instance Foldable (Four' a) where
>   foldr f z (Four' _ x x' x'') = f x'' . f x' . f x $ z


write a filter function for Foldable types using foldMap:

> filterF :: (Applicative f, Foldable t, Monoid (f a))
>        => (a -> Bool) -> t a -> f a 
> filterF = undefined

foo f = fmap (\x -> if f x then Just x else Nothing) 

filterF' f = foldr (\x acc -> if f x then pure x `mappend` acc else acc) mempty 

> f :: (Applicative f, Monoid (f a)) => (a -> Bool) -> a -> f a
> f pred x = if pred x then pure x else mempty

> filterF' pred x = foldMap (\x -> if pred x then pure x else mempty) x


> filter' :: (a -> Bool) -> [a] -> [a]
> filter' = filterF' 


specialize the types:

Monoid (f a), Foldable t, Applicative f) => (a -> Bool) -> t a -> f a

(Monoid [] a), Foldable [], Applicative []) => (a -> Bool) -> [] a -> [] a


