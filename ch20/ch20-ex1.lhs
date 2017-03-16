> import Data.Monoid
> import Data.Semigroup (Min)
> import Data.List (uncons)

1. implement the following with foldMap or foldr

> sum' :: (Foldable t, Num a) => t a -> a
> sum' = foldr (+) 0

> sum'' :: (Foldable t, Num a) => t a -> a
> sum'' = getSum . (foldMap Sum)

> product' :: (Foldable t, Num a) => t a -> a
> product' = foldr (*) 1

> product'' :: (Foldable t, Num a) => t a -> a
> product'' = getProduct . foldMap Product

> elem' :: (Foldable t, Eq a) => a -> t a -> Bool
> elem' e = foldr (\x acc -> if x == e then True else acc ) False

> elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
> -- elem'' e = getAny . foldMap (\x -> if e == x then Any True else Any False)
> elem'' e = getAny . foldMap (Any . ((==) e))

> minimum' :: (Foldable t, Ord a) => t a -> Maybe a
> minimum' = foldr (\x acc -> if acc == Nothing then Just x else min x <$> acc) Nothing

fancy implementation: 
minimum'' = fmap getMin . alaf Option foldMap (Just . Min) :: (Ord a, Foldable t) => t a -> Maybe a

> minimum'' xs = foldr (\x acc -> min x <$> acc) (fst <$> (uncons . toList' $ xs)) xs 

> maximum' :: (Foldable t, Ord a) => t a -> Maybe a
> maximum' = foldr (\x acc -> if acc == Nothing then Just x else max x <$> acc) Nothing

> null' :: (Foldable t) => t a -> Bool
> null' = foldr (\_ _ -> False) True 

> length' :: (Foldable t) => t a -> Int
> length' = foldr (\_ acc -> acc + 1) 0 

> toList' :: (Foldable t) => t a -> [a]
> toList' = foldr (\x acc -> x : acc) [] 

> fold' :: (Foldable t, Monoid m) => t m -> m
> fold' = foldMap id

define in terms of foldr: 

> foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
> foldMap' f = foldr (\x acc -> f x <> acc) mempty
