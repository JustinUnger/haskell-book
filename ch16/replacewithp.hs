replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "foo", Nothing, Just "bar"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap replaceWithP :: Functor f => (a -> Char) -> f a -> f Char

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP


-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- (.) fmap :: Functor f => (a -> b -> c) -> a -> f b -> f c
-- (.) fmap fmap :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
