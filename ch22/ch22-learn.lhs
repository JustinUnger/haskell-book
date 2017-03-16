> import Control.Applicative

> boop = (*2)

> doop = (+10)

> bip :: Integer -> Integer
> bip = boop . doop

> bloop :: Integer -> Integer
> bloop = fmap boop doop

> bbop :: Integer -> Integer
> bbop = (+) <$> boop <*> doop

turns into this: 

((+) <$> (*2) <*> (+10)) 3

(3*2) + (3+10) 

6 + 13

19

> duwop :: Integer -> Integer
> duwop = liftA2 (+) boop doop

using do notation: 

> boopDoop :: Integer -> Integer
> boopDoop = do
>   a <- boop
>   b <- doop
>   return (a + b)

equivalent to: 

> boopDoop' :: Integer -> Integer
> boopDoop' = 
>   boop >>= 
>           \a -> 
>               doop >>= 
>                        \b -> 
>                           return (a + b)

also looks like something like: 

> boopDoop'' :: Integer -> Integer 
> boopDoop'' = \x -> (boop x) + (doop x)

