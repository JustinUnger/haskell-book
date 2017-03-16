
(.) :: (b -> c) -> (a -> b) -> a -> c
--       fmap        fmap 

fmap :: Functor f => (m -> n) -> f      m    -> f      n
                     Int->Char   Maybe  Int     Maybe Char

fmap :: Functor g => (x ->   y) ->  g    x -> g    y
                    Double Char    [] Double  [] Char

((m -> n) -> f m -> f n) -> ( (x -> y) -> g x -> g y) -> f (g m) -> f (g y)
((Int -> Char) -> Maybe Int -> Maybe Char) ->
   m     n          f    m      f     n
((Double -> Int) -> [] Double -> [] Int) -> 
   x         y      g   x        g   y  
Maybe ([] Int) -> Maybe ([] 
   f   g   m       f      g  y



-- (fmap . fmap) f functor 

is same as fmap f (fmap f functor) ? 

-- > fmap (fmap (\x -> 1)) [Just 1, Nothing, Just 2]
-- [Just 1,Nothing,Just 1]

-- > (fmap . fmap) (\x -> 1) [Just 1, Nothing, Just 2]
-- [Just 1,Nothing,Just 1]

> fmapTwice :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
> fmapTwice f = fmap (fmap f)

fmap           :: Functor f => (a -> b) -> f a -> f b
id             :: (b -> b)
fmap id        :: Functor f => f b -> f b

fmap (fmap id) :: (Functor f, Functor g) => g (f b) -> g (f b)

> :t fmap . fmap id
fmap . fmap id :: Functor f => (a -> b) -> f a -> f b

> :t (fmap . fmap) id
(fmap . fmap) id :: (Functor f1, Functor f) => f (f1 b) -> f (f1 b)

> :t (fmap . fmap)
(fmap . fmap)
  :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)

> :t (.) fmap
(.) fmap :: Functor f => (a -> a1 -> b) -> a -> f a1 -> f b

((x -> y) -> g x -> g y) -> (x -> y) -> f (g x) -> f (g y)


(.) :: (b -> c) -> (a -> b) -> a -> c

         fmap

b ~ (x -> y) 
c ~ (g x -> g y)

      ( (x -> y) -> (g x -> g y) ) -> (a -> (x -> y)) -> a -> (g x -> g y)
                                   -> (a -> (x -> y)) -> a -> (g x -> g y)
                                   -> (a -> x -> y) -> a -> g x -> g y
a ~ (m -> n)
x ~ f m
y ~ f n
                                          fmap
                                     ( (m -> n) -> (f m -> f n) -> (m -> n) ) -> g (f m) -> g (f y)

      ( (x -> y) -> (g x -> g y) ) -> ( (m -> n) -> (x -> y) ) -> (m -> n) -> (g x -> g y)

(m -> n) -> g (f m) -> g (f n)

