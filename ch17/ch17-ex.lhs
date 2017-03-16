1. -- Type
   []

> lPure :: a -> [] a
> lPure = pure

> lAp :: [] (a -> b) -> [] a -> [] b
> lAp = (<*>)

2. -- Type
   IO

> ioPure :: a -> IO a
> ioPure = pure

> ioAp :: IO (a -> b) -> IO a -> IO b
> ioAp = (<*>)

3. -- Type
   (,) a

> tPure :: Monoid a => a -> (,) a a
> tPure = pure

> tAp :: Monoid a => (,) a (a -> b) -> (,) a a -> (,) a b
> tAp = (<*>)

4. -- Type
    (->) e

> fPure :: a -> (->) e a
> fPure = pure

> fAp :: (->) e (a -> b) -> (->) e a -> (->) e b
> fAp = (<*>)
