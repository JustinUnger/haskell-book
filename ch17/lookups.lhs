for problem 3:

> import Data.List (elemIndex)

1. added :: Maybe Integer
   added = (+3) (lookup 3 $ zip [1,2,3] [4,5,6])

> added :: Maybe Integer
> added = fmap (+3) (lookup 3 $ zip [1,2,3] [4,5,6])


2. tupled :: Maybe (Integer, Integer)
   tupled = (,) y z

> y :: Maybe Integer
> y = lookup 3 $ zip [1,2,3] [4,5,6]

> z :: Maybe Integer
> z = lookup 2 $ zip [1,2,3] [4,5,6]

> tupled :: Maybe (Integer, Integer)
> tupled = pure (,) <*> y <*> z

equivalent to: 

> tupled' = (,) <$> y <*> z

(,) <$> Just 1  :: Num a => Maybe (b -> (a, b))
(,) <$> Nothing :: Maybe (b -> (a, b))


3. maxed :: Maybe Int
   maxed = max' x' y'

> x' :: Maybe Int
> x' = elemIndex 3 [1,2,3,4,5]

> y' :: Maybe Int
> y' = elemIndex 4 [1,2,3,4,5]

> max' :: Int -> Int -> Int
> max' = max

> maxed :: Maybe Int
> maxed = max' <$> x' <*> y' 

4. summed :: Maybe Integer
   summed = sum $ (,) x y

> xs = [1,2,3]
> ys = [4,5,6]

> x'' :: Maybe Integer
> x'' = lookup 3 $ zip xs ys

> y'' :: Maybe Integer
> y'' = lookup 2 $ zip xs ys

> summed :: Maybe Integer
> summed = fmap sum $ (,) <$> x'' <*> y''
