> import Data.Char

> cap :: [Char] -> [Char]
> cap xs = map toUpper xs

> rev :: [Char] -> [Char] 
> rev xs = reverse xs

> composed :: [Char] -> [Char]
> composed = rev . cap

> fmapped :: [Char] -> [Char]
> fmapped = fmap rev cap

> tupled :: [Char] -> ([Char], [Char])
> tupled = (,) <$> cap <*> rev

((,) <$> cap <*> rev) "foobar"

(,) (cap "foobar") (rev "foobar")
(,) "FOOBAR" "raboof"
("FOOBAR", "raboof")


> tupledDo :: [Char] -> ([Char], [Char])
> tupledDo = do
>   c <- cap
>   r <- rev
>   return (c, r)

> tupledBind :: [Char] -> ([Char], [Char])
> tupledBind =
>   cap >>= (\c -> rev >>= \r -> return (c, r))


(,) :: a -> b -> (a, b)
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 (,) :: Applicative f => f a -> f b -> f (a, b)
liftA2 (,) cap :: ([Char] -> b) -> [Char] -> ([Char], b)

(,) :: a -> b -> (a, b)
(,) rev :: b -> ([Char] -> [Char], b)
^ -- this is a partially applied tuple constructor function, where a ~ [Char] -> [Char]
 
(,) . rev :: [Char] -> b -> ([Char], b)
^ -- this is a composed function

(.) :: (b -> c) -> (a -> b) -> a -> c

f . g = \x -> f (g x)

((,) . rev) == \x -> (,) (rev x)

