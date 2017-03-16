
> j :: Monad m => m (m a) -> m a
> j m = m >>= id

m ~ Maybe

Maybe (Maybe a) -> Maybe a

m ~ []

[] ([] a) -> [] a
[[a]] -> [a]


