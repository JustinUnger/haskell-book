> f :: Integer -> Maybe Integer
> f 0 = Nothing
> f n = Just n

> g :: Integer -> Maybe Integer
> g i = 
>     if even i
>     then Just (i + 1)
>     else Nothing

> h :: Integer -> Maybe String
> h i = Just ("10191" ++ show i)

> doSomething' n = do
>     a <- f n
>     b <- g a
>     c <- h b
>     pure (a, b, c)

> doSomething n =
>   f n >>=
>       \a -> 
>           g a >>=
>               \b ->
>                   h b >>= 
>                       \c -> pure (a,b,c)

> doSomethingApp n =
>   pure (f n) <*> 

